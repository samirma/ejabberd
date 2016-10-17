require 'xmpp4r/iq'
require 'xmpp4r/query'
require 'xmpp4r/version/iq/version'
require 'xmpp4r'
require 'xmpp4r/client'
include Jabber

userString = 'someeuser@citiviti.com'
password = '12345'

if ARGV.size == 2
  userString = ARGV[0]
  password = ARGV[1]
end


def post_text(text, longitude, latitude, cl)
	iqr = Jabber::Iq.new(:set, 'citiviti.com')
	qr = Jabber::IqQuery.new
	qr.add_namespace('jabber:iq:post')
	postXml = REXML::Element.new('post')
	postXml.add_text(text)
	postXml.add_attribute("latitude",latitude)
	postXml.add_attribute("longitude", longitude)
	qr.add(postXml)
	iqr.add(qr)
	cl.send iqr
end


# Building up the connection

Jabber::debug = true

jid = Jabber::JID.new(userString)

cl = Jabber::Client.new(jid)
cl.connect
cl.auth(password)


puts "################### Posting "


post_text("Vol Patria St 1",  "-43.18146228790283",  "-22.950371342583857", cl)

post_text("Vol Patria St 2" ,"-43.18420886993408" ,"-22.951398814282815", cl)

post_text("Vol Patria St 3", "-43.187642097473145","-22.952544831203557", cl)

post_text("Vol Patria St 4" ,"-43.19296360015869" ,"-22.954599733786548", cl)

post_text("Vol Patria St 5" ,"-43.19815635681152" ,"-22.95625944000894", cl)

post_text("Post from Corcovado",  "-43.15850257873535",  "-22.94890915786787", cl)

post_text("Post from Santos Dumont Airport" ,"-43.16408157348633" ,"-22.9131795215742", cl)

post_text("Post from Ipanema","-43.201589584350586", "-22.986723284962448", cl)

post_text("Post from Rocinha" ,"-43.247809410095215","-22.988264075194788", cl)



puts "################# Get Posting list"


iqr = Jabber::Iq.new(:get, 'citiviti.com')
qr = Jabber::IqQuery.new
qr.add_namespace('jabber:iq:post')


postXml = REXML::Element.new('post')
postXml.add_attribute("longitude","-43.17777156829834")
postXml.add_attribute("latitude","-22.942072246721743")
postXml.add_attribute("within","2000")
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
postXml.add_attribute("latitude","-118.4079")
postXml.add_attribute("longitude","31.9434")
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
