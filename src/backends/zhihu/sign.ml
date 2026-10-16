(* ZSE v4, ported from the user's zhihu.el; see NOTICE. *)
let round_keys = [|0x45c62932l;0x3d15f2fel;0x5442e14fl;0xeb8921c0l;0xd256542el;0xae28cbdel;0xf7782b08l;0xee48a883l;0x733e8d1al;0xc61cdffbl;0xe7c6016al;0x1b713876l;0xdf5eeb0al;0x8f44a6cal;0x9beb07a3l;0x7e564e94l;0x870bcbcbl;0x794d026cl;0xa54f723al;0xffaabf19l;0xfb5d9cc3l;0x832a8363l;0xb5e884fal;0x5e2b60cfl;0x4ec93b52l;0x1b3a7714l;0xad0d330fl;0xf2551fdfl;0x13ab7196l;0xd0f96adel;0x15ab9f7dl;0x8be5d87bl|]
let substitution = [|20;223;245;7;248;2;194;209;87;6;227;253;240;128;222;91;237;9;125;157;230;93;252;205;90;79;144;199;159;197;186;167;39;37;156;198;38;42;43;168;217;153;15;103;80;189;71;191;97;84;247;95;36;69;14;35;12;171;28;114;178;148;86;182;32;83;158;109;22;255;94;238;151;85;77;124;254;18;4;26;123;176;232;193;131;172;143;142;150;30;10;146;162;62;224;218;196;229;1;192;213;27;110;56;231;180;138;107;242;187;54;120;19;44;117;228;215;203;53;239;251;127;81;11;133;96;204;132;41;115;73;55;249;147;102;48;122;145;106;118;74;190;29;16;174;5;177;129;63;113;99;31;161;76;246;34;211;13;60;68;207;160;65;111;82;165;67;169;225;57;112;244;155;51;236;200;233;58;61;47;100;137;185;64;17;70;234;163;219;108;170;166;59;149;52;105;24;212;78;173;45;0;116;226;119;136;206;135;175;195;25;92;121;208;126;139;3;75;141;21;130;98;241;40;154;66;184;49;181;46;243;88;101;183;8;23;72;188;104;179;210;134;250;201;164;89;216;202;220;50;221;152;140;33;235;214|]
let alphabet = "6fpLRqJO8M/c3jnYxFkUVC4ZIG12SiH=5v0mXDazWBTsuw7QetbKdoPyAl+hN9rgE"
let version="101_3_3.0"
let key="059053f7d15e01d7"
let rot x n=Int32.logor(Int32.shift_left x n)(Int32.shift_right_logical x (32-n))
let xor=Int32.logxor
let g x=
 let s=ref 0x0l in
 for i=0 to 3 do let b=Int32.to_int(Int32.logand 0xffl (Int32.shift_right_logical x (24-8*i))) in
 s:=Int32.logor !s (Int32.shift_left (Int32.of_int substitution.(b)) (24-8*i)) done;
 List.fold_left xor !s (List.map(rot !s)[2;10;18;24])
let block input=
 let state=Array.make 36 0x0l in
 for i=0 to 3 do for j=0 to 3 do
 state.(i)<-Int32.logor state.(i)(Int32.shift_left(Int32.of_int input.(i*4+j))(24-j*8)) done done;
 for i=0 to 31 do state.(i+4)<-xor state.(i)(g(xor (xor state.(i+1)state.(i+2))(xor state.(i+3)round_keys.(i)))) done;
 Array.init 16(fun i->Int32.to_int(Int32.logand 0xffl(Int32.shift_right_logical state.(35-i/4)(24-(i mod 4)*8))))
let encode input=
 let n=Array.length input in let padded=Array.init(n+(3-n mod 3) mod 3)(fun i->if i<n then input.(i) else 0) in
 let out=Buffer.create 64 and index=ref 0 in
 for group=Array.length padded/3-1 downto 0 do
  let value=ref 0 in
  for j=0 to 2 do let mask=58 lsr (8*(!index mod 4)) land 255 in incr index;
   value:= !value lor ((padded.(group*3+2-j) lxor mask) lsl (j*8)) done;
  for i=0 to 3 do Buffer.add_char out alphabet.[(!value lsr (6*i)) land 63] done
 done;Buffer.contents out
let encrypt input=
 (* Header inputs are hexadecimal MD5, all in encodeURIComponent's unescaped set. *)
 let n=2+String.length input in let padding=16-n mod 16 in
 let plain=Array.make(n+padding)padding in plain.(0)<-12;plain.(1)<-0;
 String.iteri(fun i c->plain.(i+2)<-Char.code c)input;
 let cipher=Array.make(Array.length plain)0 and iv=ref(Array.init 16(fun i->Char.code key.[i] lxor 42)) in
 for chunk=0 to Array.length plain/16-1 do
  iv:=block(Array.init 16(fun i->plain.(chunk*16+i) lxor !iv.(i)));
  Array.blit !iv 0 cipher (chunk*16) 16
 done;encode cipher
let header ~url ~dc0 =
 let u=Uri.of_string url in
 let path=match Uri.path_and_query u with ""->"/"|s->s in
 "2.0_"^encrypt(Digest.to_hex(Digest.string(version^"+"^path^"+"^dc0)))
