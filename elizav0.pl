%% Projeto ELIZA em Prolog 
%% Por Thais Nascimento
%% Ultima atualização em 29/11/2021


%% [Inicializacao] - inicializa o script atraves da funcao main
%%:- initialization(main).

:- dynamic(base_respostas/2).
%% [DCG]

oi --> [oi].
computador --> [computador].
nome --> [nome].
desculpe --> [desculpe].
%% meupai --> [meu, pai].
sair --> [bye].
sonho --> [sonho].
minha --> [minha].

%% Base de respostas

%% base_respostas(_,_).

%% Regras

%% nova_resp(P,S) :- [P,S].

sublist([P|R], L) :-
    append([_, [P|R], _], L).
    
sublist(T, L) :-
    append([_, [T], _], L).


frase(meupai, L) --> [meu, pai], palavras(L).
frase(meupai, L) --> palavras(_), [meu, pai], palavras(L).

frase(sonhei_com, L) --> [eu, sonhei, com], palavras(L).
frase(sonhei_com, L) --> palavras(_), [eu, sonhei, com], pronome(minha,L), palavras(L).
frase(sonhei_com, L) --> palavras(_), [eu, sonhei, com], palavras(L).

frase(estou_feliz, L) --> [estou, feliz], palavras(L).
frase(estou_feliz, L) --> palavras(_), [estou, feliz], palavras(L).

frase(estou_triste, L) --> [estou, triste], palavras(L).
frase(estou_triste, L) --> palavras(_), [estou, triste], palavras(L).

frase(estou_triste, L) --> [estou, triste], palavras(L).
frase(estou_triste, L) --> palavras(_), [estou, triste], palavras(L).

palavras([]) --> [].
palavras([P|R]) --> [P], palavras(R).

nova_resp_sonhei_com(sonhei_com, L, P, S) --> palavras(_), [eu, sonhei, com], pronome(minha, P), subst(palavra,S), palavras(L).

%% nova_resp_sonhei_com(sonhei_com, L, P, S) :- 
%%    palavras(_), [eu, sonhei, com], pronome(minha, P), subst(palavra,S), palavras(L),
    

/*
pega_curinga2(E, newList) :-
    insert()
*/
subst(palavra, S) --> [S].

pronome(minha, L) --> [sua].

% member(Item, List) - succeeds if the item is a member of the list;
%   List must be instantiated at the time member is called. Item need not
%   be instantiated.

%% Interpretador de linguagem natural
%% Nesta parte, o objetivo é interpretar a entrada do usuário e descobrir com qual regra ela se adequa

interpretar(E,eh_computador) :-sublist(computador,E).

interpretar(E,meu_pai) :- 
    frase(meupai, _, E, []).

/*
interpretar(E,tem_sonhei_com) :- 
    frase(sonhei_com, L, E, []),
    nova_resp_sonhei_com(L, nova_resp).

*/
/* sonhei com -
 Entrada  E = [eu, sonhei, com, minha, viagem]
 Como você se sente em relação a [2] na verdade?
 Como você se sente em relação a [sua viagem] na verdade?
*/

% Examples of use:
% ?- member(b, [a, b, c]).
% true.
% 
% ?- member(X, [a, b, c]).
% X = a ;
% X = b ;
% X = c ;
% false.


/*
interpretar(E,tem_sonhei_com) :- 
    frase(sonhei_com, L, E, []).
*/

interpretar(E,tem_sonhei_com) :- 
    frase(sonhei_com, L, E, []),
    asserta(base_respostas(L,sonhei_com)).

interpretar(E,tem_estou_feliz) :- 
    frase(estou_feliz, _, E, []).

interpretar(E,tem_estou_triste) :- 
    frase(estou_triste, _, E, []).

interpretar(E,eh_oi) :- sublist(oi,E).

interpretar(E,tem_nome) :- sublist(nome,E).

interpretar(E,tem_desculpe):- sublist(desculpe,E).

interpretar(E,tem_sonho) :- sublist(sonho,E). 

interpretar(E,tem_parece) :- sublist(parece,E). 

interpretar(E,tem_mesmo) :- sublist(mesmo,E). 

interpretar(E,tem_sou) :- sublist(sou,E). 

interpretar(E,frase_sair) :- sair(E,[]). 

interpretar(_,frase_nao_reconhecida).

%% teste

write_name((_, Name)) :-
  writeln(Name).

responder(meu_pai) :-
    random_member(Resp, ['Seu pai?',
                        'Ele influencia você fortemente?',
                        'O que mais vem à mente quando você pensa no seu pai?']),
    enumere(Resp),
    interagir.

responder(tem_estou_feliz) :-
    random_member(Resp, ['Eu tenho alguma influência nisso?',
                        'O que te faz feliz?',
                        'Você pode explicar o porque de feliz?']),
    enumere(Resp),
    interagir.    

responder(tem_estou_triste) :-
    random_member(Resp, ['Sinto que você se sinta assim',
                        'Estou certo de que não é prazeroso estar assim']),
    enumere(Resp),
    interagir.    

responder(tem_sonhei_com):-
  %%   asserta(base_respostas(P,S)),
    call(base_respostas,Resp,S),
   %% enumere(L),
   %% Res = term_string(S),
    write("Como você se sente em relação a "), write(Resp), write(" na verdade?"), nl,nl,
    interagir.


responder(eh_oi) :-
    write("Como vai você? Por favor, me fale do seu problema."),nl,nl,
    interagir.

responder(tem_nome) :-
    write("Não estou interessado em nomes"),nl,nl,
    interagir.

responder(eh_computador) :-
    random_member(Resp, ['Computadores te incomodam?',
                'O que você acha sobre máquinas?',
                'Porque você menciona computadores?',
                'O que você acha que máquinas tem a ver com o seu problema?']),
    enumere(Resp),
    interagir.

responder(tem_desculpe) :-
    random_member(Resp, ['Por favor, não se desculpe',
                'Desculpas não são necessárias',
                'Como você se sente quando se desculpa?']),
    enumere(Resp),
    interagir.

responder(tem_sonho) :-
    random_member(Resp, ['O que este sonho sugere a você?',
                        'Você sonha com frequencia?',
                        'Que pessoas aparecem em seus sonhos?',
                        'Você não acha que sonhos tem algo a ver com o seu problema?']),
    enumere(Resp),
    interagir.

responder(tem_parece) :-
    random_member(Resp, ['De que forma?',
                'Que similaridades há?']),
    enumere(Resp),
    interagir.

responder(tem_mesmo) :-
    write('Que outras conexões você observa?'),
    interagir.

responder(tem_sou) :-
    random_member(Resp, ['Porque você está dizendo "SOU"?',
                'Nao entendi']),
    enumere(Resp),
    interagir.

responder(frase_nao_reconhecida) :-
    random_member(Resp, ['Muito interessante.',
                        'Não sei se entendi você direito',
                        'O que isso sugere a você?',
                        'Por favor, continue.',
                        'Continue',
                        'Você quer mesmo falar sobre isso?',
                        'Elabore melhor']),
    enumere(Resp),
    interagir.

responder(frase_sair) :-
    write("Tchau"), nl, nl.

%% Manipulando entradas e saidas 

normalize_string([], []).% include space before interrogation

normalize_string([63|R], [32,63|NR]) :- % 63 == '?'
    normalize_string(R, NR).

normalize_string([44|R], [32,44|NR]) :- % 44 == ','
    normalize_string(R, NR).

normalize_string([H|R], [H|NR]) :-
    normalize_string(R, NR).

enumere(L):-
    write(L),nl,nl.

prompt(L) :-
    write('> '),
    read_line_to_codes(user_input, Cs), %% lê o teclado
    normalize_string(Cs, NCs),
    atom_codes(A, NCs), %% A ~atomos aka palavras
    string_lower(A, AP),
    atomic_list_concat(L, ' ', AP). %% aqui vira uma lista

%% Laço de interação com o usuário 

interagir :-
    prompt(E),
    interpretar(E,Pergunta),
    responder(Pergunta).