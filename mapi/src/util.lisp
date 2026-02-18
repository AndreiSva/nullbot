(in-package #:org.rm-r.mapi.util)

(defun randint (start end)
  (+ start (random (+ 1 (- end start)))))

(defun rand-string (len &aux (arr (make-array len)))
  (loop for i from 0 below len do
    (setf (aref arr i) (randint 65 90)))
  (fs:octets-to-string arr))

(defgeneric sendmsg (obj room-id content)
  (:method ((obj matrix-client) room-id content
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
