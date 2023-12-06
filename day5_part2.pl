:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- set_prolog_flag(double_quotes, string).
:- set_prolog_flag(debug_on_error, true).

:- initialization(main, main).

main([Filename]) :-
    phrase_from_file(day5input(SeedRanges, Rangemaps), Filename),
    store_inputs(Rangemaps),
    minimum_seed_location(SeedRanges, Location),
    writeln(Location).
             
day5input(SeedRanges, Rangemaps) -->
    "seeds: ", seed_ranges(SeedRanges),
    eol,
    rangemaps(Rangemaps).
    
seed_range(From, To) --> integer(From), " ", integer(Len), (" " | eol),
    { To is From + Len }.
seed_ranges([]) --> [].
seed_ranges([seed_range(From, To)|Rs]) --> seed_range(From, To), seed_ranges(Rs).
    
space_separated_integers([]) --> [].
space_separated_integers([N]) --> integer(N), [].
space_separated_integers([N|Ns]) --> integer(N), " ", space_separated_integers(Ns).    
    
translation(translation(DestDomainMin, SrcDomainMin, Len)) -->
    space_separated_integers([DestDomainMin, SrcDomainMin, Len]),
    eol.    

translations([]) --> eol.
translations([T|Ts]) --> translation(T), translations(Ts).

rangemap(rangemap(From, To, Translations)) -->
    as_atom(From),
    "-to-",
    as_atom(To),
    " map:",
    eol,
    translations(Translations)
    .
    
rangemaps([]) --> [].
rangemaps([R|Rs]) --> rangemap(R), rangemaps(Rs).

as_atom(S) --> dcg_basics:string(Codes), { atom_codes(S, Codes) }.

:- dynamic map/3.

store_inputs(Rangemaps) :-
    member(rangemap(From, To, Translations), Rangemaps),
    assertz(map(From, To, Translations)).
   
applicable_translation([], _, _) :- fail.
applicable_translation([T|_], SrcDomainValue, T) :-
    T = translation(_, SrcDomainMin, Len),
    SrcDomainValue #>= SrcDomainMin,
    SrcDomainValue #=< SrcDomainMin + Len.
applicable_translation([_|Ts], SrcDomainValue, T) :-
    applicable_translation(Ts, SrcDomainValue, T).
    
translated_value_single(From, To, FromValue, ToValue) :-
    map(From, To, Translations),
    (
        applicable_translation(Translations, FromValue, translation(DestDomainMin, SrcDomainMin, _))
        -> ToValue #= DestDomainMin + FromValue - SrcDomainMin
        ; ToValue = FromValue
    ).
    
translated_value(From, To, FromValue, ToValue) :-
    translated_value_single(From, To, FromValue, ToValue).
translated_value(From, To, FromValue, ToValue) :-
    translated_value_single(From, Intermediate, FromValue, IntermediateValue),
    translated_value(Intermediate, To, IntermediateValue, ToValue).
        
minimum_seed_location(SeedRanges, Location) :-
    findall(L, (
        member(seed_range(SeedRangeStart, SeedRangeEnd), SeedRanges),
        Seed #>= SeedRangeStart,
        Seed #=< SeedRangeEnd,
        translated_value(seed, location, Seed, L)
    ), Locations),
    min_list(Locations, Location).
    
