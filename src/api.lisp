;; readers beware: this is currently a very barebones library

(defpackage nullbot/matrix-api
  (:use #:cl
        #:cl-hash-util)
  (:local-nicknames
   (:jzon :com.inuoe.jzon)
   (:fs :flexi-streams))
  (:export
   #:matrix-user
   #:homeserver
   #:name
   #:listening
   #:token
   #:lock
   #:matrix-bot
   #:sendmsg
   #:on-event
   #:start
   #:stop))
(in-package #:nullbot/matrix-api)

(defclass matrix-user ()
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

(defclass matrix-bot (matrix-user) ()
  (:default-initargs :name "matrix-bot"))

(defgeneric request (obj endpoint &rest rest)
  (:method ((obj matrix-user) endpoint &rest rest &aux (headers))
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
  (:method ((obj matrix-user) event room-id)
    (format t "Event Received: ~a~%" event)))

(defun randint (start end)
  (+ start (random (+ 1 (- end start)))))

(defun rand-string (len &aux (arr (make-array len)))
  (loop for i from 0 below len do
    (setf (aref arr i) (randint 65 90)))
  (fs:octets-to-string arr))

(defgeneric sendmsg (obj room-id content)
  (:method ((obj matrix-user) room-id content
            &aux
              (msg (make-hash-table :test #'equal))
              (encoded-room-id (quri:url-encode room-id))
              (unique-str (rand-string 20)))
    (setf (gethash "msgtype" msg) "m.text")
    (setf (gethash "body" msg) content)
    (request obj (format nil "/rooms/~a/send/m.room.message/~a"
                         encoded-room-id
                         unique-str)
             :put
             msg
             '(("Content-Type" . "application/json")))))

(defgeneric get-events (obj rooms-join room-id)
  (:method ((obj matrix-user) rooms-join room-id
            &aux
              (room-table (gethash room-id rooms-join))
              (events
               (hash-get room-table '("timeline" "events"))))
    (when events
      (loop for event across events do
        (on-event obj event room-id)))))

(defgeneric start (obj)
  (:method-combination progn)
  (:method ((obj matrix-user))
    (setf (listening obj) t)
    (bt2:make-thread (lambda (&aux
                                (since)
                                (sync-route "/sync?timeout=30000"))
                       (loop while (bt2:with-lock-held ((lock obj)) (listening obj)) do
                         (when since
                           (setf sync-route (format nil "/sync?timeout=30000&since=~a" since)))
                         (let* ((response (request obj sync-route :get))
                                (rooms-join (hash-get response '("rooms" "join"))))
                           (when rooms-join (loop for room-id being each hash-key of rooms-join
                                                  do (when since (get-events obj rooms-join room-id))))
                           (setf since (gethash "next_batch" response))))
                       (format t "Shutting down...~%"))
                     :name (format nil "~a Poll Thread" (name obj)))))

(defgeneric stop (obj)
  (:method ((obj matrix-user))
    (bt2:with-lock-held ((lock obj)) (setf (listening obj) nil))))
