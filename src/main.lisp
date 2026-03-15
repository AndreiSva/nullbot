(defpackage nullbot
  (:use #:cl
        #:cl-hash-util)
  (:local-nicknames
   (:jzon :com.inuoe.jzon)
   (:mapi :org.rm-r.mapi)
   (:sseq :split-sequence)
   (:dex :dexador))
  (:import-from #:org.rm-r.mapi.util #:sendmsg)
  (:export
   #:start))
(in-package #:nullbot)

(defclass nullbot (mapi:matrix-bot) ())
(defparameter *bot* (make-instance 'nullbot
                                   :token (uiop:getenv "NULLBOT_TOKEN")
                                   :homeserver "matrix.nullring.xyz"))

(defparameter +feed-url+ "https://list.nullring.xyz/discussion/new.atom")
(defparameter +feed-room-id+ "!ShuXi5ohrPUtKHkrNO:matrix.nullring.xyz")
(defparameter +feed-cache-path+ #P"./nullbot_cache.sexp")
(defparameter +feed-sleep-minutes+ 1)

(defparameter +prefix+ "$")

(defun get-temp (weather-station
    &aux
       (endpoint (format nil "https://api.weather.gc.ca/collections/swob-realtime/items?f=json&lang=en&url=C~A&sortby=-date_tm-value&limit=1&properties=date_tm-value,air_temp,air_temp-uom,air_temp-qa" weather-station))
       (data (jzon:parse (dex:get endpoint))))
  (hash-get (aref (gethash "features" data) 0) '("properties" "air_temp")))

(defun process-roommsg
    (content room-id sender
     &aux
       (msgtype (gethash "msgtype" content))
       (body (gethash "body" content))
       (split-body (sseq:split-sequence #\Space body))
       (command (car split-body)))
  (format t "processing msg~%")
  (when (and (> (length body) 0) (equal (aref (car split-body) 0) #\$))
    (cond
      ((string= command "$help")
       (sendmsg *bot* room-id "Unlike some other bots, I'm nice :3"))
      ((string= command "$weather")
       (sendmsg *bot* room-id (format nil "It's ~a degrees in Vancouver~%It's ~a degrees in Victoria" (get-temp "YVR") (get-temp "YYJ")))))))

(defmethod mapi:on-event
    ((obj nullbot) event
     &aux
       (msgtype (event-get event "type"))
       (sender (event-get event "sender")))
  (cond
    ((string= msgtype "m.room.message")
     (process-roommsg (event-get event "content") (room-id event) sender))))

(defun node-val (obj)
  (car (xmls:node-children obj)))

(defun node-attr (obj name)
  (cdr (assoc name (xmls:node-attrs obj) :test #'string=)))

;; TODO: make this into a generic f-n maybe and also make it not dumb
(defun get-node-by-name (obj name)
  (check-type obj xmls:node)
  (check-type name string)
  (loop for child in (xmls:node-children obj)
        when (and (xmls:node-p child) (string= name (xmls:node-name child)))
          return child))

(defun send-entry (entry)
  (sendmsg
   *bot*
   +feed-room-id+
   (format nil "New message on mailing list!~%Title: ~a~%From: ~a~%Link: ~a~%"
           (getf entry :title)
           (getf entry :author)
           (getf entry :link))))

(defun write-entries (entries)
  (with-open-file (str +feed-cache-path+
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
    (format str "~s" entries)))

(defun feed-thread ()
  (loop while (bt2:with-lock-held ((mapi:lock *bot*)) (mapi:listening-p *bot*)) do
    (format t "Doing another poll~%")
    (let* ((feed-str (dex:get +feed-url+))
           (xmlobj (xmls:parse feed-str))
           (entries (loop for entry in (xmls:node-children xmlobj)
                          when (string= (xmls:node-name entry) "entry")
                            collect `(:id ,(node-val (get-node-by-name entry "id"))
                                      :title ,(node-val (get-node-by-name entry "title"))
                                      :author ,(node-val (node-val (get-node-by-name entry "author")))
                                      :link ,(node-attr (get-node-by-name entry "link") "href"))))
           (cached-entries))

      (if (uiop:file-exists-p +feed-cache-path+)
          (setf cached-entries (read-from-string (uiop:read-file-string +feed-cache-path+)))
          (write-entries entries))

      (when cached-entries
        (loop for entry in entries
              when (not (find (getf entry :id)
                              cached-entries
                              :test #'string=
                              :key (lambda (e) (getf e :id))))
                do (send-entry entry)))
      ;; update the cache with the new entries
      (write-entries entries))
    (sleep (* 60 +feed-sleep-minutes+))))

(defun start ()
  (bt2:make-thread #'feed-thread :name "nullbot polling thread")
  (mapi:start *bot*))
