require 'xmpp4r/iq'
require 'xmpp4r/query'
require 'xmpp4r/version/iq/version'
require 'xmpp4r'
require 'xmpp4r/client'
include Jabber

number = rand(1000)

userString = "anonymous_#{number}@citiviti.com"
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


iqr = Jabber::Iq.new(:get, 'citiviti.com')
qr = Jabber::IqQuery.new
qr.add_namespace('jabber:iq:citivitiregister')

postXml = REXML::Element.new('register')
postXml.add_attribute("request_number","552199991111")
qr.add(postXml)
iqr.add(qr)

#puts "##################### Register request"
cl.send iqr


iqr = Jabber::Iq.new(:set, 'citiviti.com')
qr = Jabber::IqQuery.new
qr.add_namespace('jabber:iq:citivitiregister')

postXml = REXML::Element.new('register')
postXml.add_attribute("request_number","552199991111")
postXml.add_attribute("code","4")
qr.add(postXml)
iqr.add(qr)

puts "##################### Register request"

cl.send iqr


sleep(10)

cl.close
