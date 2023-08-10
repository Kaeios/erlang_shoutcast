-module(lib_files_find).
-export([files/3, files/5]).

-include_lib("kernel/include/file.hrl").

files(Dir, Ext, Flag) -> 
    lists:reverse(files(Dir, Ext, Flag, fun(File, Acc) ->[File|Acc] end, [])).

files(Dir, Ext, Recursive, Fun, Acc) ->
    case file:list_dir(Dir) of
	{ok, Files} -> find_files(Files, Dir, Ext, Recursive, Fun, Acc);
	{error, _}  -> Acc
    end.

find_files([File|T], Dir, Ext, Recursive, Fun, Acc0) ->
    FullName = Dir ++  [$/|File],
    case file_type(FullName) of
	regular ->
	    case filename:extension(FullName) of
		Ext  -> 
		    Acc = Fun(FullName, Acc0),
		    find_files(T, Dir, Ext, Recursive, Fun, Acc);
		_ ->
		    find_files(T, Dir, Ext, Recursive, Fun, Acc0)
	    end;
	directory -> 
	    case Recursive of
		true ->
		    Acc1 = files(FullName, Ext, Recursive, Fun, Acc0),
		    find_files(T, Dir, Ext, Recursive, Fun, Acc1);
		false ->
		    find_files(T, Dir, Ext, Recursive, Fun, Acc0)
	    end;
	error -> 
	    find_files(T, Dir, Ext, Recursive, Fun, Acc0)
    end;
find_files([], _, _, _, _, A) ->
    A.

file_type(File) ->
    case file:read_file_info(File) of
	{ok, Facts} ->
	    case Facts#file_info.type of
		regular   -> regular;
		directory -> directory;
		_         -> error
	    end;
	_ ->
	    error
    end.