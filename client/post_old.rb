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


puts "################### Posting "

iqr = Jabber::Iq.new(:set, 'citiviti.com')
qr = Jabber::IqQuery.new
qr.add_namespace('jabber:iq:post')


postXml = REXML::Element.new('post')
postXml.add_text("Post from Corcovado")
postXml.add_attribute("latitude","-22.94890915786787")
postXml.add_attribute("longitude","-43.15850257873535")
qr.add(postXml)
iqr.add(qr)
cl.send iqr





puts "################# Get Posting list"


iqr = Jabber::Iq.new(:get, 'citiviti.com')
qr = Jabber::IqQuery.new
qr.add_namespace('jabber:iq:post')


postXml = REXML::Element.new('post')
postXml.add_attribute("latitude","-22.949991966204575")
postXml.add_attribute("longitude","-43.15472602844238")
qr.add(postXml)
iqr.add(qr)
cl.send iqr


puts "################################## Comenting"

iqr = Jabber::Iq.new(:set, 'citiviti.com')
qr = Jabber::IqQuery.new
qr.add_namespace('jabber:iq:comment')
postXml = REXML::Element.new('comment')
postXml.add_attribute("post_id","1")
postXml.add_text("My comment")
postXml.add_attribute("latitude","-22.949991966204575")
postXml.add_attribute("longitude","-43.15472602844238")
qr.add(postXml)
iqr.add(qr)
cl.send iqr

puts "################# Get Commenting list"


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
