(defpackage org.rm-r.mapi
  (:use #:cl
        #:cl-hash-util)
  (:local-nicknames
   (#:jzon #:com.inuoe.jzon))
  (:export
   #:matrix-user
   #:homeserver
   #:username
   #:listening-p
   #:token
   #:lock
   #:matrix-client
   #:matrix-bot

   #:on-event
   #:on-sync
   #:start
   #:stop
   #:whoami
   #:request
   #:login
   #:join
   #:leave
   #:directory-room
   #:sync

   #:event
   #:data
   #:make-event
   #:event-get
   #:id
   #:find-events

   #:room-id
   #:room-id-p

   #:room-alias
   #:room-alias-p))

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
