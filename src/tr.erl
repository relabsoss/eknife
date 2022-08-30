-module(tr).

-export([slugify/1, translit/1]).

-spec slugify(list() | binary()) -> binary().

slugify([]) -> [];
slugify(<<>>) -> <<>>;
slugify(Str) when is_list(Str) ->
    binary_to_list(re:replace(re:replace(translit(string:lowercase(Str)),
                                         "^-*|-*$",
                                         "",
                                         [global, {return, binary}]),
                              "-_ ",
                              "",
                              [global, {return, binary}]));
slugify(Str) when is_binary(Str) -> translit(Str, <<>>).

-spec translit(binary()) -> binary().

translit(Str) ->
    translit(unicode:characters_to_binary(Str), <<>>).

translit(<<>>, Acc) -> Acc;
translit(<<"ä"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "a">>);
translit(<<"ë"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "e">>);
translit(<<"ï"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "i">>);
translit(<<"ü"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "u">>);
translit(<<"ö"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "o">>);
translit(<<"Ä"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "A">>);
translit(<<"Ë"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "E">>);
translit(<<"Ï"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "I">>);
translit(<<"Ü"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "U">>);
translit(<<"Ö"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "O">>);
translit(<<"é"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "e">>);
translit(<<"è"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "e">>);
translit(<<"É"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "E">>);
translit(<<"È"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "E">>);
translit(<<"í"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "i">>);
translit(<<"ì"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "i">>);
translit(<<"Í"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "I">>);
translit(<<"Ì"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "I">>);
translit(<<"ú"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "u">>);
translit(<<"ù"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "u">>);
translit(<<"Ú"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "U">>);
translit(<<"Ù"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "U">>);
translit(<<"ó"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "o">>);
translit(<<"ò"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "o">>);
translit(<<"Ó"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "O">>);
translit(<<"Ò"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "O">>);
translit(<<"ß"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "ss">>);
translit(<<"ç"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "c">>);
translit(<<"Ç"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "C">>);
translit(<<"ø"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "o">>);
translit(<<"Ø"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "O">>);
translit(<<"å"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "a">>);
translit(<<"Å"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "A">>);
translit(<<"€"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "E">>);
translit(<<"ÿ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "ij">>);
translit(<<"@"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "at">>);
% Cyrillic support (from http://en.wikipedia.org/wiki/Romanization_of_Russian)
translit(<<"А"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "A">>);
translit(<<"а"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "a">>);
translit(<<"Б"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "B">>);
translit(<<"б"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "b">>);
translit(<<"В"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "V">>);
translit(<<"в"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "v">>);
translit(<<"Г"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "G">>);
translit(<<"г"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "g">>);
translit(<<"Д"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "D">>);
translit(<<"д"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "d">>);
translit(<<"Е"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "E">>);
translit(<<"е"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "e">>);
translit(<<"Ё"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "Yo">>);
translit(<<"ё"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "yo">>);
translit(<<"Ж"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "Zh">>);
translit(<<"ж"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "zh">>);
translit(<<"З"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "Z">>);
translit(<<"з"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "z">>);
translit(<<"И"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "I">>);
translit(<<"и"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "i">>);
translit(<<"Й"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "J">>);
translit(<<"й"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "j">>);
translit(<<"К"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "K">>);
translit(<<"к"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "k">>);
translit(<<"Л"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "L">>);
translit(<<"л"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "l">>);
translit(<<"М"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "M">>);
translit(<<"м"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "m">>);
translit(<<"Н"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "N">>);
translit(<<"н"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "n">>);
translit(<<"О"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "O">>);
translit(<<"о"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "o">>);
translit(<<"П"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "P">>);
translit(<<"п"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "p">>);
translit(<<"Р"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "R">>);
translit(<<"р"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "r">>);
translit(<<"С"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "S">>);
translit(<<"с"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "s">>);
translit(<<"Т"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "T">>);
translit(<<"т"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "t">>);
translit(<<"У"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "U">>);
translit(<<"у"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "u">>);
translit(<<"Ф"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "F">>);
translit(<<"ф"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "f">>);
translit(<<"Х"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "H">>);
translit(<<"х"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "h">>);
translit(<<"Ц"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "C">>);
translit(<<"ц"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "c">>);
translit(<<"Ч"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "Ch">>);
translit(<<"ч"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "ch">>);
translit(<<"Ш"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "Sh">>);
translit(<<"ш"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "sh">>);
translit(<<"Щ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "Sch">>);
translit(<<"щ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "sch">>);
translit(<<"Ъ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "'">>);
translit(<<"ъ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "'">>);
translit(<<"Ы"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "Y">>);
translit(<<"ы"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "y">>);
translit(<<"Ь"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "'">>);
translit(<<"ь"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "'">>);
translit(<<"Э"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "He">>);
translit(<<"э"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "he">>);
translit(<<"Ю"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "Yu">>);
translit(<<"ю"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "yu">>);
translit(<<"Я"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "Ya">>);
translit(<<"я"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "ya">>);
% Ukrainian support
translit(<<"Ґ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "G">>);
translit(<<"ґ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "g">>);
translit(<<"Ї"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "I">>);
translit(<<"ї"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "i">>);
translit(<<"І"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "I">>);
translit(<<"і"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "i">>);
translit(<<"Є"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "Ye">>);
translit(<<"є"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "ye">>);
% Polish support
translit(<<"Ą"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "A">>);
translit(<<"ą"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "a">>);
translit(<<"Ę"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "E">>);
translit(<<"ę"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "e">>);
translit(<<"Ć"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "C">>);
translit(<<"ć"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "c">>);
translit(<<"Ł"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "L">>);
translit(<<"ł"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "l">>);
translit(<<"Ń"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "N">>);
translit(<<"ń"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "n">>);
translit(<<"Ś"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "S">>);
translit(<<"ś"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "s">>);
translit(<<"Ź"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "Z">>);
translit(<<"ź"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "z">>);
translit(<<"Ż"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "Z">>);
translit(<<"ż"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "z">>);
% Turkish support
translit(<<"Ş"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "S">>);
translit(<<"ş"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "s">>);
translit(<<"Ğ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "G">>);
translit(<<"ğ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "g">>);
translit(<<"İ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "I">>);
translit(<<"ı"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "i">>);
% Hebrew support (simplified) https://en.wikipedia.org/wiki/Romanization_of_Hebrew
translit(<<"א"/utf8, T/binary>>, Acc) ->
    translit(T, Acc);
translit(<<"ב"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "v">>);
translit(<<"בּ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "b">>);
translit(<<"ג"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "g">>);
translit(<<"גּ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "g">>);
translit(<<"ג׳"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "j">>);
translit(<<"ד"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "d">>);
translit(<<"דּ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "d">>);
translit(<<"ד׳"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "dh">>);
translit(<<"ה"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "h">>);
translit(<<"הּ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "h">>);
translit(<<"ו"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "v">>);
translit(<<"וּ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "v">>);
translit(<<"ז"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "z">>);
translit(<<"זּ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "z">>);
translit(<<"ז׳"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "zh">>);
translit(<<"ח"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "ch">>);
translit(<<"ט"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "t">>);
translit(<<"טּ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "t">>);
translit(<<"י"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "y">>);
translit(<<"יּ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "y">>);
translit(<<"ךכ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "ch">>);
translit(<<"ךּ כּ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "k">>);
translit(<<"ל"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "l">>);
translit(<<"לּ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "l">>);
translit(<<"םמ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "m">>);
translit(<<"מּ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "m">>);
translit(<<"ןנ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "n">>);
translit(<<"נּ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "n">>);
translit(<<"ס"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "s">>);
translit(<<"סּ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "s">>);
translit(<<"ע"/utf8, T/binary>>, Acc) ->
    translit(T, Acc);
translit(<<"ףפ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "f">>);
translit(<<"ףּ פּ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "p">>);
translit(<<"ץצ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "tz">>);
translit(<<"צּ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "tz">>);
translit(<<"ץ׳צ׳"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "tsh">>);
translit(<<"ק"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "k">>);
translit(<<"קּ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "k">>);
translit(<<"ר"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "r">>);
translit(<<"רּ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "r">>);
translit(<<"ש"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "sh">>);
translit(<<"שׁ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "sh">>);
translit(<<"שּׁ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "sh">>);
translit(<<"שׂ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "s">>);
translit(<<"שּׂ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "s">>);
translit(<<"ת"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "t">>);
translit(<<"תּ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "t">>);
translit(<<"ת׳"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "th">>);
% Hebrew forms used in translitearion from Arabic
translit(<<"ח׳"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "n">>);
translit(<<"ט׳"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "z">>);
translit(<<"ע׳ר׳"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "g">>);
% Hebrew vowels
translit(<<"צ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "aa">>);
translit(<<"טְ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "e">>);
translit(<<"חֱ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "e">>);
translit(<<"חֲ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "a">>);
translit(<<"חֳ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "o">>);
translit(<<"טִ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "i">>);
translit(<<"טֵ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "e">>);
translit(<<"טֶ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "e">>);
translit(<<"טַ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "a">>);
translit(<<"טָ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "o">>);
translit(<<"טֹ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "o">>);
translit(<<"טֻ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "u">>);
translit(<<"טוּ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "u">>);
translit(<<"טֵי"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "ei">>);
translit(<<"טֶי"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "ei">>);
translit(<<"טַיטַיְ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "ai">>);
translit(<<"טָיטָיְ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "ai">>);
translit(<<"טֹיטֹיְ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "oi">>);
translit(<<"טֻיטֻיְ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "ui">>);
translit(<<"טוּיטוּיְ"/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, "ui">>);
translit(<<Any/utf8, T/binary>>, Acc) ->
    translit(T, <<Acc/binary, Any/utf8>>).
