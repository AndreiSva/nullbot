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
    :accessor listening-p)
   (shutting-down
    :type boolean
    :initform nil
    :accessor shutting-down-p)
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

(defclass event ()
  ((data
    :type hash-table
    :initarg :data
    :initform (make-hash-table :test #'equal)
    :accessor data)
   (room-id
    :type string
    :initarg :room-id
    :accessor room-id)))

(defun make-event (&optional init-table room-id &aux (e (make-instance 'event)))
  (setf (data e) init-table)
  (setf (room-id e) room-id)
  e)

(defgeneric event-get (obj &rest rest)
  (:documentation "Get some data from an event. Returns nil if a given path is not in the event")
  (:method ((obj event) &rest rest)
    (hash-get (data obj) rest)))

(defgeneric id (obj)
  (:documentation "Returns a string containing the ID of a given event")
  (:method ((obj event))
    (event-get obj "event_id")))

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
                                                      (content (jzon:stringify (cadr rest)))
                                                      (max-tries 3))
    (declare (type string endpoint))
    (when (member method '(:put :post))
      (push '("Content-Type" . "application/json") headers))
    (when (>= (length rest) 3) (setf headers (car (last rest))))
    (bt2:with-lock-held ((lock obj))
      (push `("Authorization" . ,(format nil "Bearer ~a" (token obj))) headers))

    (loop for try from 1 to max-tries do
      (handler-case
          (progn
            (return (jzon:parse (dexador:request (format nil "https://~a/_matrix/client/v3~a"
                                                         (homeserver obj) endpoint)
                                                 :headers headers
                                                 :method method
                                                 :content content
                                                 :verbose nil))))
        ;; something has gone wrong... we should probably try again a few times
        ;; log the error and we will continue
        (error (c)
          (sleep 1)
          ;; we tried our best
          (when (= try max-tries)
            (signal c)))))))

(defgeneric on-sync (obj sync-data)
  (:documentation
   "Runs whenever a sync is completed")
  (:method ((obj matrix-client) sync-data)))

(defgeneric on-event (obj event)
  (:documentation
   "Method that triggers every time an event is received via /sync.

Does not run for the first call to /sync.

You can use this to build your own architecture for listening for events.")
  (:method ((obj matrix-client) event)
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

(defgeneric sync (obj &key timeout since set-presence)
  (:method ((obj matrix-client) &key timeout since set-presence
            &aux (params))
    (when timeout
      (push `("timeout" . ,timeout) params))
    (when since
      (push `("since" . ,since) params))
    (when set-presence
      (push `("set_presence" . ,set-presence) params))
    (let ((result (request obj (format nil "/sync?~a"
                                       (quri:url-encode-params params))
                           :get)))
      (on-sync obj result)
      result)))

(defgeneric leave (obj room-id)
  (:method ((obj matrix-client) room-id)
    (check-type room-id room-id)
    (request obj (format nil "/rooms/~a/leave"
                         (quri:url-encode room-id))
             :post)))

(defgeneric trigger-event-hooks (obj events)
  (:method ((obj matrix-client) events)
    (when events
      (loop for event in events do
        (on-event obj event)))))

(defun find-events (table &key parent-room-id)
  "Traverse a Matrix /sync response and return a flat list of event objects found."
  ;; TODO: It might be a good idea to hard-code the schema of /sync instead of
  ;; doing this heuristic approach. Right now we do it because it's more terse and
  ;; possibly somewhat resilient to API changes.
  (cond
    ((hash-table-p table)
     (if (gethash "event_id" table)
         (list (make-event table parent-room-id))
         (loop for key being the hash-keys in table
                 using (hash-value value)
               for room-id-to-use = (or parent-room-id
                                        (when (room-id-p key)
                                          key))
               if (hash-table-p value)
                 append (find-events value :parent-room-id room-id-to-use)
               else if (and (vectorp value) (not (stringp value)))
                      append (find-events value :parent-room-id room-id-to-use))))
    ((and (vectorp table)
          (not (stringp table)))
     (loop for item across table append (find-events item :parent-room-id parent-room-id)))))

(defgeneric sync-loop (obj)
  (:method ((obj matrix-client)
            &aux
              (since))
    (loop while (bt2:with-lock-held ((lock obj)) (listening-p obj)) do
      (let* ((response (sync obj
                             :timeout 30000
                             :since since
                             :set-presence "online"))
             (events-list (find-events response)))
        (when since
          (trigger-event-hooks obj events-list))
        (setf since (gethash "next_batch" response))))
    (setf (shutting-down-p obj) nil)))

(defgeneric start (obj)
  (:documentation "Start a matrix-client by starting its listening loop")
  (:method ((obj matrix-client) &aux
                                  (listening)
                                  (shutting-down))

    (bt2:with-lock-held ((lock obj))
      (setf listening (listening-p obj))
      (setf shutting-down (shutting-down-p obj)))

    (unless (or listening shutting-down)
      (bt2:with-lock-held ((lock obj)) (setf (listening-p obj) t))
      (bt2:make-thread (lambda () (sync-loop obj))
                       :name (format nil "~a Poll Thread" (name obj))))))

(defgeneric stop (obj)
  (:documentation "Stop a matrix-client

Note that it will not shut down immediately, but rather this method sets a signal which
will cause the the matrix-client to shut down after it has finished its last sync loop.")
  (:method ((obj matrix-client))
    (bt2:with-lock-held ((lock obj))
      (setf (listening-p obj) nil)
      (setf (shutting-down-p obj) t))))
