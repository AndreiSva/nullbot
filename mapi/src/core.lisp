(in-package #:org.rm-r.mapi)

(defclass matrix-client ()
  ((homeserver
    :type string
    :initarg :homeserver
    :initform "matrix.org"
    :reader homeserver)
   (name
    :type string
    :initarg :name
    :initform "matrix-user"
    :reader name)
   (listening
    :type boolean
    :initform nil
    :accessor listening)
   (token
    :type string
    :initarg :token
    :initform ""
    :reader token)
   (lock
    :type bt2:lock
    :initform (bt2:make-lock)
    :reader lock)))

;; these are not perfect functions by any means but matrix has
;; many different room versions with different formats
;; this is what the official matrix-bot-sdk does as well
(defun room-id-p (object)
  (and (stringp object)
       (> (length object) 0)
       (equal (aref object 0) #\!)))

(defun room-alias-p (object)
  (and (stringp object)
       (> (length object) 0)
       (equal (aref object 0) #\#)))

(deftype room-id ()
  '(and string (satisfies room-id-p)))

(deftype room-alias ()
  '(and string (satisfies room-alias-p)))

(defclass matrix-bot (matrix-client) ()
  (:default-initargs :name "matrix-bot"))

(defgeneric request (obj endpoint &rest rest)
  (:method ((obj matrix-client) endpoint &rest rest &aux (headers))
    (declare (type string endpoint))

    (when (>= (length rest) 3) (setf headers (car (last rest))))
    (bt2:with-lock-held ((lock obj))
      (push `("Authorization" . ,(format nil "Bearer ~a" (token obj))) headers))
    (jzon:parse (dexador:request (format nil "https://~a/_matrix/client/v3~a"
                                         (homeserver obj) endpoint)
                                 :headers headers
                                 :method (car rest)
                                 :content (jzon:stringify (cadr rest))
                                 :verbose nil))))

(defgeneric on-event (obj event room-id)
  (:method ((obj matrix-client) event room-id)
    (format t "Event Received: ~a~%" event)))

(defgeneric whoami (obj)
  (:method ((obj matrix-client))
    (request obj "/account/whoami" :get)))

(defgeneric directory-room (obj room-alias)
  (:method ((obj matrix-client) room-alias)
    (check-type room-alias room-alias)
    (request obj
             (format nil "/directory/room/~a"
                     (quri:url-encode room-alias))
             :get)))

(defgeneric join (obj room)
  (:method ((obj matrix-client) room)
    (request obj (format nil "/join/~a"
                         (quri:url-encode room))
             :post
             (make-hash-table)
             '(("Content-Type" . "application/json")))))

(defgeneric sync (obj &key timeout since)
  (:method ((obj matrix-client) &key timeout since
            &aux (params))
    (check-type timeout integer)
    (check-type since integer)
    (when timeout
      (push '("timeout" . timeout) params))
    (when since
      (push '("since" . since) params))
    (request obj (format nil "/sync?~a"
                         (quri:url-encode-params params))
             :get)))

(defgeneric leave (obj room-id)
  (:method ((obj matrix-client) room-id)
    (check-type room-id room-id)
    (request obj (format nil "/rooms/~a/leave"
                         (quri:url-encode room-id))
             :post)))

(defgeneric get-events (obj rooms-join room-id)
  (:method ((obj matrix-client) rooms-join room-id
            &aux
              (room-table (gethash room-id rooms-join))
              (events
               (hash-get room-table '("timeline" "events"))))
    (when events
      (loop for event across events do
        (on-event obj event room-id)))))

(defgeneric start (obj)
  (:method ((obj matrix-client))
    (unless (listening obj)
      (setf (listening obj) t)
      (bt2:make-thread (lambda (&aux
                                  (since)
                                  ;; TODO: support a configurable timeout
                                  (timeout 30000)
                                  (sync-route (format nil "/sync?timeout=~a" timeout)))
                         (loop while (bt2:with-lock-held ((lock obj)) (listening obj)) do
                           (when since
                             (setf sync-route (format nil "/sync?timeout=~a&since=~a" timeout since)))
                           (let* ((response (request obj sync-route :get))
                                  (rooms-join (hash-get response '("rooms" "join"))))
                             (when rooms-join (loop for room-id being each hash-key of rooms-join
                                                    do (when since (get-events obj rooms-join room-id))))
                             (setf since (gethash "next_batch" response))))
                         (format t "Shutting down...~%"))
                       :name (format nil "~a Poll Thread" (name obj))))))

(defgeneric stop (obj)
  (:method ((obj matrix-client))
    (bt2:with-lock-held ((lock obj)) (setf (listening obj) nil))))
