module Utils = struct
	open Str;;

	let read_file (filename : string) : string =
		let ic = open_in filename in
		let n = in_channel_length ic in
		let s = Bytes.create n in
			really_input ic s 0 n;
			close_in ic;
			Bytes.to_string s;;
end;;