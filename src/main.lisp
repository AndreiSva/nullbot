(defpackage nullbot
  (:use #:cl
        #:cl-hash-util)
  (:local-nicknames
   (:jzon :com.inuoe.jzon)
   (:mapi :nullbot/matrix-api)
   (:sseq :split-sequence))
  (:export
   #:start))
(in-package #:nullbot)

(defclass nullbot (mapi:matrix-bot) ())
(defparameter *bot* (make-instance 'nullbot
                                   :token (uiop:getenv "NULLBOT_TOKEN")
                                   :homeserver "matrix.nullring.xyz"))

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
       (mapi:sendmsg *bot* room-id "Unlike some other bots, I'm nice :3")))))

(defmethod mapi:on-event
    ((obj nullbot) event room-id
     &aux
       (msgtype (gethash "type" event))
       (sender (gethash "sender" event)))
  (cond
    ((string= msgtype "m.room.message")
     (process-roommsg (gethash "content" event) room-id sender))))
