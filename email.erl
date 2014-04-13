-module(email).
-export([send/1]).
-include("email.hrl").

-define(MAX_SIZE, 1024).
-define(DE, io:format("~p:~p~n", [?FILE, ?LINE])).

%% send email by email record
send(Email) when
        undefined =/= Email#email.server_ip,
        undefined =/= Email#email.account,
        undefined =/= Email#email.password ->
    {ok, Sock} = gen_tcp:connect(Email#email.server_ip,
                                 Email#email.server_port,
                                 [binary, {active, false}, {packet, 0}]),
    connect_email(Sock, Email),
    send_email_head(Sock, Email),
    send_email_info(Sock, Email),
    send_email_data(Sock, Email),
    gen_tcp:close(Sock).

%% connect your email
connect_email(Sock, Email) ->
   send_socket(Sock, "HELO "),
   send_socket(Sock, Email#email.account),
   send_socket(Sock, "\r\n"),
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
    send_socket(Sock, "MAIL FROM <"),
    send_socket(Sock, Email#email.account),
    send_socket(Sock, ">\r\n"),
    recv_socket(Sock),

    rcpt_to_emails(Sock, Email#email.to_emails),
    recv_socket(Sock).

%% send email info
send_email_info(Sock, Email) ->
    send_socket(Sock, "DATA\r\n"),
    recv_socket(Sock),

    send_socket(Sock, "FROM:<"),
    send_socket(Sock, Email#email.account),
    send_socket(Sock, ">\r\n"),
    recv_socket(Sock),

    send_socket(Sock, "SUBJECT:"),
    case Email#email.subject of
        undefined -> send_socket(Sock, " ");
        Subject   -> send_socket(Sock, unicode:characters_to_list(Subject))
    end,
    send_socket(Sock, "\r\n"),
    send_socket(Sock, "MIME-VERSION: 1.0\r\n"),
    send_socket(Sock, "CONTENT-TYPE: multipart/mixed; BOUNDARY=\"#BOUNDARY#\"\r\n"),
    send_socket(Sock, "\r\n").

%% send email data
send_email_data(Sock, Email) ->
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
    end,

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
    send_file_to_email(Sock, Fd, 0),
    ok = file:close(Fd),
    send_socket(Sock, "\r\n\r\n"),
    send_email_attachment(Type, Rest, Sock).

%% send file
send_file_to_email(Sock, Fd, Base64Flag) ->
    case file:read(Fd, ?MAX_SIZE) of
        {ok, Data} ->
            case Base64Flag of
                -1 -> ok = gen_tcp:send(Sock, Data);
                0  -> ok = gen_tcp:send(Sock, base64:encode(Data))
            end,
            send_file_to_email(Sock, Fd, Base64Flag);
        eof             -> eof;
        {error, Reason} -> io:format("read failed: ~p~n", [Reason])
    end.

%% her email address
rcpt_to_emails(_Sock, []) ->
    ok;
rcpt_to_emails(Sock, [ToEmail | Rest]) ->
    send_socket(Sock, "RCPT TO <"),
    send_socket(Sock, ToEmail),
    send_socket(Sock, ">\r\n"),
    rcpt_to_emails(Sock, Rest).

%% send socket
send_socket(Sock, Data) when is_list(Data)->
    ok = gen_tcp:send(Sock, unicode:characters_to_binary(Data));
send_socket(Sock, Data) when is_binary(Data)->
    ok = gen_tcp:send(Sock, Data).

%% recv socket
recv_socket(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {ok   , Packet} -> io:format("~p~n", [binary_to_list(Packet)]);
        {error, Reason} -> io:format("recv failed: ~p~n", [Reason])
    end.
