-record(email, {
          server_ip  , % (必填   ) 邮件服务器ip(如: "smtp.exmail.qq.com")
          account    , % (必填   ) 你自己的邮箱名(如: "281754179@qq.com")
          password   , % (必填   ) 密码(如: "password1989")
          to_emails  , % (必填   ) 要发往的邮箱(格式:["","",...])(如: ["281754179@qq.com", "leptune@live.cn"])
          ssl = false, % (optional) 服务器是否需要ssl加密。是则取值为true
          server_port, % (optional) 默认端口是25, 如ssl为true，则默认端口为465(如: 25)
          subject    , % (optional) 邮件标题(如: "email test")
          text       , % (optional) (正文)将文本内容发往邮箱，text的值为存放该内容的文件路径(相对或绝对, 路径以'/'分隔，不要以'\\'分隔)(如: "../test.txt")
          html       , % (optional) (正文)将网页内容发往邮箱，html值为该网页文件路径(注: 两个正文内容最多只能选一项。若都选了，那只会显示html)(如: "test.html")
          attachment   % (optional) (附件)要发往邮箱的文件路径(格式:["""",...],如:["test.doc","test.tar"])(注意后缀名要对，否则不能预览)(如: ["/home/leptune/test.tar"])
}).

-record(socket, {type, sock}).

-define(SSL_SERV_PORT_DEF, 465).
-define(NOT_SSL_SERV_PORT_DEF, 25).
