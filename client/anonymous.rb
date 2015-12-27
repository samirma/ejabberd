require 'xmpp4r/iq'
require 'xmpp4r/query'
require 'xmpp4r/version/iq/version'
require 'xmpp4r'
require 'xmpp4r/client'
include Jabber

userString = 'anonymous_user1@citiviti.com'
password = '12345'

if ARGV.size == 2
  userString = ARGV[0]
  password = ARGV[1]
end



# Building up the connection

Jabber::debug = true

jid = Jabber::JID.new(userString)

cl = Jabber::Client.new(jid)
cl.connect
cl.auth(password)




sleep(10)

cl.close
