erlang_email
============
using erlang to send email (text,html,attachments) with smtp

compile: erlc -W *.erl

Example:(in the erlang shell) 

1> rr("email.hrl").

2> E = #email{

2> server_ip="smtp.exmail.qq.com", 

2> account="your qq email",

2> password="your qq password",

2> subject="email subject",

2> to_emails=["281754179@qq.com","leptune@live.cn"],

2> html="test.html",

2> attachment=["test.html", "test.doc", "test.txt", "test.tar"]}.

3> email:send(E).
