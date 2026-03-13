(defpackage org.rm-r.mapi
  (:use #:cl
        #:cl-hash-util)
  (:local-nicknames
   (#:jzon #:com.inuoe.jzon))
  (:export
   #:matrix-user
   #:homeserver
   #:name
   #:listening-p
   #:token
   #:lock
   #:matrix-client
   #:matrix-bot
   #:on-event
   #:start
   #:stop
   #:whoami
   #:request
   #:join
   #:leave
   #:room-id))

(defpackage org.rm-r.mapi.commands
  (:use #:cl
        #:cl-hash-util)
  (:local-nicknames
   (#:mapi #:org.rm-r.mapi)))

(defpackage org.rm-r.mapi.util
  (:use #:cl
        #:org.rm-r.mapi)
  (:export
   #:sendmsg)
  (:local-nicknames
   (#:fs #:flexi-streams)))
