-module(email).
-include("email.hrl").

-export([send/1]).
-compile(export_all).

-define(MAX_SIZE, 1024).
-define(DE, io:format("~p:~p~n", [?FILE, ?LINE])).

%% send email by email record
send(Email) when
        undefined =/= Email#email.server_ip,
        undefined =/= Email#email.account,
        undefined =/= Email#email.to_emails,
        undefined =/= Email#email.password ->
    ServerPort =
        case Email#email.server_port of
            undefined -> case Email#email.ssl of
                             true  -> ?SSL_SERV_PORT_DEF;
                             false -> ?NOT_SSL_SERV_PORT_DEF
                         end;
            Any       -> Any
        end,
    Sock =
        case Email#email.ssl of
            false -> {ok, Socket} =
                         gen_tcp:connect(Email#email.server_ip,
                                         ServerPort,
                                         [binary, {active, false}, {packet, 0}]),
                     #socket{type = tcp, sock = Socket};
            true  -> ok = ssl:start(),
                     {ok, Socket} =
                         ssl:connect(Email#email.server_ip,
                                     ServerPort,
                                     [binary, {active, false}, {packet, 0}],
                                     infinity),
                     #socket{type = ssl, sock = Socket}
        end,
    connect_email(Sock, Email),
    send_email_head(Sock, Email),
    send_email_info(Sock, Email),
    send_email_data(Sock, Email),
    end_email(Sock),
    case Sock#socket.type of
        ssl -> ssl:close(Sock#socket.sock),
               ssl:stop();
        tcp -> gen_tcp:close(Sock#socket.sock)
    end.

%% connect your email
connect_email(Sock, Email) ->
   send_socket(Sock, "HELO " ++ Email#email.account ++ "\r\n"),
   recv_socket(Sock),

   send_socket(Sock, "AUTH LOGIN\r\n"),
   recv_socket(Sock),

   send_socket(Sock, base64:encode(Email#email.account)),
   send_socket(Sock, "\r\n"),
   recv_socket(Sock),

   send_socket(Sock, base64:encode(Email#email.password)),
   send_socket(Sock, "\r\n"),
   recv_socket(Sock).

%% send email head
send_email_head(Sock, Email) ->
    send_socket(Sock, "MAIL FROM <" ++ Email#email.account ++ ">\r\n"),
    recv_socket(Sock),

    rcpt_to_emails(Sock, Email#email.to_emails),
    recv_socket(Sock).

%% send email info
send_email_info(Sock, Email) ->
    send_socket(Sock, "DATA\r\n"),
    recv_socket(Sock),

    send_socket(Sock, "FROM:<" ++ Email#email.account ++ ">\r\n"),
    recv_socket(Sock),

    Subject = unicode:characters_to_list(Email#email.subject),
    send_socket(Sock, "SUBJECT:"++ Subject ++ "\r\n").

%% send email data
send_email_data(Sock, Email) when Email#email.text       =/= undefined;
                                  Email#email.html       =/= undefined;
                                  Email#email.attachment =/= undefined ->
    send_socket(Sock, "MIME-VERSION: 1.0\r\n"),
    send_socket(Sock, "CONTENT-TYPE: multipart/mixed; BOUNDARY=\"#BOUNDARY#\"\r\n"),
    send_socket(Sock, "\r\n"),
    case Email#email.text of
        undefined -> nothing_to_do;
        _         -> send_email_text("text/plain", Email#email.text, Sock)
    end,
    case Email#email.html of
        undefined -> nothing_to_do;
        _         -> send_email_text("text/html", Email#email.html, Sock)
    end,
    case Email#email.attachment of
        undefined -> nothing_to_do;
        _         -> send_email_attachment("application/msword", Email#email.attachment, Sock)
    end;
send_email_data(_Sock, _Email) ->
    ok.

end_email(Sock) ->
    send_socket(Sock, "\r\n.\r\n"),
    recv_socket(Sock),
    send_socket(Sock, "QUIT\r\n"),
    recv_socket(Sock).

%% send email text
send_email_text(Type, FilePath, Sock) ->
    send_socket(Sock, "--#BOUNDARY#\r\n"),
    send_socket(Sock, "CONTENT-TYPE: "),
    send_socket(Sock, Type),
    send_socket(Sock, "\r\n\r\n"),

    {ok, Fd} = file:open(FilePath, [binary, read]),
    send_file_to_email(Sock, Fd, -1),
    ok = file:close(Fd),
    send_socket(Sock, "\r\n\r\n").

%% send email other type
send_email_attachment(_Type, [], _Sock) ->
    nothing_to_return;
send_email_attachment(Type, [FilePath | Rest], Sock) ->
    send_socket(Sock, "--#BOUNDARY#\r\n"),
    send_socket(Sock, "CONTENT-TYPE: "),
    send_socket(Sock, Type),
    send_socket(Sock, "; NAME="),
    send_socket(Sock, misc:basename(FilePath)),
    send_socket(Sock, "\r\n"),
    send_socket(Sock, "CONTENT-TRANSFER-ENCODING: base64\r\n"),
    send_socket(Sock, "\r\n"),

    {ok, Fd} = file:open(FilePath, [binary, read]),
    io:format("Client: Send ~p to server....~n", [FilePath]),
    send_file_to_email(Sock, Fd, 0),
    ok = file:close(Fd),
    send_socket(Sock, "\r\n\r\n"),
    send_email_attachment(Type, Rest, Sock).

%% send file
send_file_to_email(Sock, Fd, Base64Flag) ->
    case file:read(Fd, ?MAX_SIZE) of
        {ok, Data} ->
            case Base64Flag of
                -1 -> ok = send(Sock, Data);
                0  -> ok = send(Sock, base64:encode(Data))
            end,
            send_file_to_email(Sock, Fd, Base64Flag);
        eof             -> eof;
        {error, Reason} -> io:format("read failed: ~p~n", [Reason])
    end.

%% her email address
rcpt_to_emails(_Sock, []) ->
    ok;
rcpt_to_emails(Sock, [ToEmail | Rest]) ->
    send_socket(Sock, "RCPT TO <" ++ ToEmail ++ ">\r\n"),
    rcpt_to_emails(Sock, Rest).

%% send socket
send_socket(Sock, Data) when is_list(Data)->
    send_socket(Sock, list_to_binary(Data));
send_socket(Sock, Data) when is_binary(Data)->
    io:format("Client: ~p~n", [Data]),
    ok = send(Sock, Data).

%% recv socket
recv_socket(Sock) ->
    case recv(Sock, 0) of
        {ok   , Packet} -> io:format("Server: ~p~n", [binary_to_list(Packet)]);
        {error, Reason} -> io:format("Server: recv failed: ~p~n", [Reason])
    end.

%% send data to server via tcp or ssl
send(Sock, Data) when Sock#socket.type =:= tcp ->
    gen_tcp:send(Sock#socket.sock, Data);
send(Sock, Data) when Sock#socket.type =:= ssl ->
    ssl:send(Sock#socket.sock, Data).

%% recv data to server via tcp or ssl
recv(Sock, Opinion) when Sock#socket.type =:= tcp ->
    gen_tcp:recv(Sock#socket.sock, Opinion);
recv(Sock, Opinion) when Sock#socket.type =:= ssl ->
    ssl:recv(Sock#socket.sock, Opinion).

test() ->
    send(#email{server_ip   = "smtp.qq.com",
                account     = "965609038@qq.com",
                password    = "srbank2013",
                subject     = "smtp邮件测试",
                html        = "test.html",
                attachment  = ["test.doc", "test.html", "test.tar", "test.txt"],
                to_emails   = ["281754179@qq.com"]}).
