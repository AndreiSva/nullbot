(in-package #:org.rm-r.mapi.commands)

(defclass matrix-command-bot (matrix-bot)
  ((prefix
    :type string
    :initarg :prefix
    :initform "$"
    :reader prefix)
   (commands
    :type list
    :accessor commands))
  (:default-initargs :name "command-bot"))

(defgeneric command-add (obj command-name callback &optional arguments))

(defgeneric dispatch-command (obj command-string)
  (:method ((obj matrix-command-bot) command-string)
    ))

(defmethod on-event ((obj matrix-command-bot) event room-id
                     &aux
                       (msgtype (event-get event "type"))
                       (sender (event-get event "sender")))
  (when (string= msgtype "m.room.message")
    (let* ((body (hash-get event '("content" "body")))
           (prefix-length (length (prefix obj))))
      (when (and (>= (length body) prefix-length)
                 (string= (prefix obj) body :start1 0 :end1 prefix-length))
        (dispatch-command obj command-string)))))
