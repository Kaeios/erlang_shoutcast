-module(shout).
-export([start/0]).

-define(CHUNKSIZE, 24576).

start() ->
    mp3_manager:start(),
    spawn(fun() -> 
            start_parallel_server(3000),
            %% now go to sleep - otherwise the 
            %% listening socket will be closed
            receive
                after
                    infinity -> true
                end
	  end).

start_parallel_server(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, [binary,    {packet, 4},
                                                    {reuseaddr, true},
                                                    {active, true}
                                        ]
                                ),
%% create a song server -- this just knows about all our music
    PidSongServer = spawn(fun() -> songs() end), 
    spawn(fun() -> per_connect(Listen, PidSongServer) end).

per_connect(Listen, PidSongServer) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    io:format("Connection !~n"),
    %% when accept returns spawn a new process to
    %% wait for the next connection
    spawn(fun() -> per_connect(Listen, PidSongServer) end),
    inet:setopts(Socket, [binary,   {packet,0},
                                    {nodelay,true},
                                    {active, true}
                        ]
                ),
    %% deal with the request
    get_request(Socket, PidSongServer, []).

%% wait for the TCP request
get_request(Socket, PidSongServer, L) ->
    receive
    {tcp, Socket, Bin} ->
        L1 = L ++ binary_to_list(Bin),
        case split(L1, []) of
            more ->
                get_request(Socket, PidSongServer, L1);
            {_Request, _Rest} ->
                got_request(Socket, PidSongServer)
        end;
    {tcp_closed, Socket} ->
        void;

    _Any ->
        get_request(Socket, PidSongServer, L)
end.

got_request(Socket, PidSongServer) ->
    gen_tcp:send(Socket, response()),
    play_songs(Socket, PidSongServer, <<>>).

play_songs(Socket, PidSongServer, Data) ->
    Song = rpc(PidSongServer, next_song),
    {File, PrintStr, Header} = unpack_song_descriptor(Song),
    case id3_tag_lengths:file(File) of
        error ->
            io:format("Error : id3"),
            play_songs(Socket, PidSongServer, Data);
        {Start, Stop} ->
            io:format("Playing : ~p~n", [PrintStr]),
            {ok, Stream} = file:open(File, [read, binary, raw]),
            Data1 = send_file(Stream, {0, Header}, Start, Stop, Socket, Data),
            file:close(Stream),
            play_songs(Socket, PidSongServer, Data1)
        end.

send_file(S, Header, OffSet, Stop, Socket, SoFar) ->
    %% OffSet = first byte to play
    %% Stop   = The last byte we can play
    Need = ?CHUNKSIZE - size(SoFar),
    Last = OffSet + Need,
    if
	Last >= Stop ->
	    %% not enough data so read as much as possible and return
	    Max = Stop - OffSet,
	    {ok, Bin} = file:pread(S, OffSet, Max),
	    list_to_binary([SoFar, Bin]);
	true ->
	    {ok, Bin} = file:pread(S, OffSet, Need),
	    write_data(Socket, SoFar, Bin, Header),
	    send_file(S, bump(Header),
		      OffSet + Need,  Stop, Socket, <<>>)
    end.

write_data(Socket, B0, B1, Header) ->
    %% Check that we really have got a block of the right size
    %% this is a very useful check that our program logic is
    %% correct
    case size(B0) + size(B1) of
	?CHUNKSIZE ->
	    case gen_tcp:send(Socket, [B0, B1, the_header(Header)]) of
		ok -> true;
		{error, closed} ->
		    %% this happens if the player 
		    %% terminates the connection
		    exit(playerClosed)
	    end;
	_Other ->
	    %% don't send the block - report an error
	    io:format("Block length Error: B0 = ~p b1=~p~n",
		      [size(B0), size(B1)])
    end.

songs() ->
    {ok,[SongList]} = file:consult("mp3data.tmp"),
    songs_loop(SongList, 0).

songs_loop(SongList, CurrentIndex) ->
    receive
        {From, next_song} ->
            Song = lists:nth(CurrentIndex + 1, SongList),
            From ! {self(), Song},
            songs_loop(SongList, (CurrentIndex + 1) rem length(SongList))
        end.

rpc(Pid, Command) ->
    Pid ! {self(), Command},
    receive
	{Pid, Reply} ->
	    Reply
    end.

unpack_song_descriptor({File, {_Tag,Info}}) ->
    PrintStr = list_to_binary(make_header1(Info)),
    L1 = ["StreamTitle='",PrintStr,
	  "';StreamUrl='http://localhost:3000';"],
    %% io:format("L1=~p~n",[L1]),
    Bin = list_to_binary(L1),
    Nblocks = ((size(Bin) - 1) div 16) + 1,
    NPad = Nblocks*16 - size(Bin), 
    Extra = lists:duplicate(NPad, 0),
    Header = list_to_binary([Nblocks, Bin, Extra]),
    %% Header is the Shoutcast header
    {File, PrintStr, Header}.

response() ->
    ["ICY 200 OK\r\n",
     "icy-notice1: <BR>This stream requires",
     "<a href=\"http://www.winamp.com/\">Winamp</a><BR>\r\n",
     "icy-notice2: Erlang Tutorial Shoutcast server<BR>\r\n",
     "icy-name: Erlang mix\r\n",
     "icy-genre: Train songs\r\n",
     "icy-url: http://localhost:3000\r\n",
     "Content-Type: audio/mpeg\r\n",
     "icy-pub: 1\r\n",
     "icy-metaint: ",integer_to_list(?CHUNKSIZE),"\r\n",
     "icy-br: 96\r\n\r\n"]. 

bump({K, H}) -> {K+1, H}.

the_header({K, H}) ->
    case K rem 5 of
	0 -> H;
	_ -> <<0>>
    end.

make_header1([{track,_}|T]) -> 
    make_header1(T);    
make_header1([{Tag,X}|T]) ->
    [atom_to_list(Tag),": ",X," "|make_header1(T)];
make_header1([]) ->
    [].

split("\r\n\r\n" ++ T, L) -> {lists:reverse(L), T};
split([H|T], L)           -> split(T, [H|L]);
split([], _)              -> more.