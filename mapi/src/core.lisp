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

(defclass matrix-bot (matrix-client) ()
  (:default-initargs :name "matrix-bot"))

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

(defgeneric request (obj endpoint &rest rest)
  (:documentation
   "Make an http/https request to a Matrix homeserver

Syntax: (REQUEST obj endpoint METHOD data headers)

`endpoint` is a string path containing the matrix endpoint WITHOUT the protocol,
           hostname, and /_matrix/client/v3
`METHOD` is a symbol (e.g :get, :post, :put) that represents the HTTP method to be used
`data` is a hash table, which will be sent as json
`headers` is an alist containing additional headers to be sent.

When :post or :put are used, the application/json content-type is set automatically.

In most cases there are wrapper methods for endpoints you would want to call. Only use
this method when there isn't a wrapper function available for your endpoint.")
  (:method ((obj matrix-client) endpoint &rest rest &aux
                                                      (headers)
                                                      (method (car rest))
                                                      (content (jzon:stringify (cadr rest))))
    (declare (type string endpoint))
    (when (member method '(:put :post))
      (push '("Content-Type" . "application/json") headers))
    (when (>= (length rest) 3) (setf headers (car (last rest))))
    (bt2:with-lock-held ((lock obj))
      (push `("Authorization" . ,(format nil "Bearer ~a" (token obj))) headers))
    (jzon:parse (dexador:request (format nil "https://~a/_matrix/client/v3~a"
                                         (homeserver obj) endpoint)
                                 :headers headers
                                 :method method
                                 :content content
                                 :verbose nil))))

(defgeneric on-event (obj event room-id)
  (:documentation
   "Method that triggers every time an event is received via /sync.

Does not run for the first call to /sync.

You can use this to build your own architecture for listening for events.")
  (:method ((obj matrix-client) event room-id)
    (format t "Event Received: ~a~%" event)))

(defgeneric whoami (obj)
  (:documentation
   "Run a /_matrix/client/v3/account/whoami request, returning the resulting hash table")
  (:method ((obj matrix-client))
    (request obj "/account/whoami" :get)))

(defgeneric directory-room (obj room-alias)
  (:documentation
   "Run a /_matrix/client/v3/directory/room request, returning the resulting hash table

This can be used to map a room alias to an ID, or get a list of homeservers that have the specific room.")
  (:method ((obj matrix-client) room-alias)
    (check-type room-alias room-alias)
    (request obj
             (format nil "/directory/room/~a"
                     (quri:url-encode room-alias))
             :get)))

(defgeneric join (obj room)
  (:documentation "Run a /_matrix/client/v3/join request, returning the resulting hash table

This can be used to join matrix rooms.")
  (:method ((obj matrix-client) room)
    (request obj (format nil "/join/~a"
                         (quri:url-encode room))
             :post
             (make-hash-table))))

(defgeneric sync (obj &key timeout since)
  (:method ((obj matrix-client) &key timeout since set-presence
            &aux (params))
    (declare (type integer timeout))
    (declare (type string since))
    (when timeout
      (push '("timeout" . timeout) params))
    (when since
      (push '("since" . since) params))
    (when set-presence
      (push '("set_presence" . set-presence) params))
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
