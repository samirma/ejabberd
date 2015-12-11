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

iqr = Jabber::Iq.new(:set, 'citiviti.com')
qr = Jabber::IqQuery.new
qr.add_namespace('jabber:iq:post')


postXml = REXML::Element.new('post')
postXml.add_text("Hello post world with gps attibutes")
postXml.add_attribute("latitute","-118.4079")
postXml.add_attribute("longitude","31.9434")
qr.add(postXml)
iqr.add(qr)
#cl.send iqr

puts "##################################"

iqr = Jabber::Iq.new(:set, 'citiviti.com')
qr = Jabber::IqQuery.new
qr.add_namespace('jabber:iq:comment')


postXml = REXML::Element.new('comment')
postXml.add_attribute("post_id","1")
postXml.add_text("My comment")
qr.add(postXml)
iqr.add(qr)
cl.send iqr

puts "#################"


iqr = Jabber::Iq.new(:get, 'citiviti.com')
qr = Jabber::IqQuery.new
qr.add_namespace('jabber:iq:comment')


postXml = REXML::Element.new('comment')
postXml.add_attribute("post_id","1")
qr.add(postXml)
iqr.add(qr)
cl.send iqr

sleep(10)

cl.close
