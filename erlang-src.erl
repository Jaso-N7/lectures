%%%
% Sample Taken from: https://www.tutorialspoint.com/erlang/erlang_file_input_output.htm
%%%

% hello world program
-module(helloworld). 
% export function used to ensure the start function can be accessed.
-export([start/0]). 

start() -> 
% File - This is the location of the file that needs to be opened.
% Mode - This is the mode in which the file needs to be opened in.
%
% Open(File,Mode) - Returns a handle to the file, if the op is successful.
   {ok, File} = file:open("Newfile.txt",[read]),
% The file, which must exist, is opened for reading.
% read(FileHandler,NumberofBytes) - Returns the requested read info from the file
   Txt = file:read(File,1024 * 1024),
% Output the contents
   io:fwrite("~p~n",[Txt]).