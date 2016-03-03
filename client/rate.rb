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


def rate_post(post_id, rate, cl)
	iqr = Jabber::Iq.new(:set, 'dev.citiviti.com')
	qr = Jabber::IqQuery.new
	qr.add_namespace('jabber:iq:rate')
	postXml = REXML::Element.new('post')
	postXml.add_attribute("post_id",post_id)
	postXml.add_attribute("rate",rate)
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



puts "################# Rate"

rate_post(1, "up", cl)
rate_post(1, "up", cl)
rate_post(1, "down", cl)


sleep(10)

cl.close
