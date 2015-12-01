require 'xmpp4r/iq'
require 'xmpp4r/query'
require 'xmpp4r/version/iq/version'
require 'xmpp4r'
require 'xmpp4r/client'
include Jabber

userString = 'user3@citiviti.com'
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


iq = Jabber::Iq.new(:get, 'citiviti.com')
# and ask for the version
iq.query = Jabber::Version::IqQueryVersion.new

cl.send(iq)

puts "##################################"

iqr = Jabber::Iq.new(:get, 'citiviti.com')
qr = Jabber::IqQuery.new
qr.add_namespace('jabber:iq:post')
qr.add(REXML::Element.new('post').add_text("My super post!!!!"))
iqr.add(qr)

cl.send iqr

sleep(10)

cl.close
