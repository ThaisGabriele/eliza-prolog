%% Projeto ELIZA em Prolog 
%% Por Thais Nascimento
%% Ultima atualização em 29/11/2021


%% [Inicializacao] - inicializa o script atraves da funcao main
%%:- initialization(main).

%% [DCG]

oi --> [oi].
computador --> [computador].
nome --> [nome].
desculpe --> [desculpe].
%% meupai --> [meu, pai].
sair --> [bye].


%% Regras

sublist([P|R], L) :-
    append([_, [P|R], _], L).
    
sublist(T, L) :-
    append([_, [T], _], L).


frase(meupai, L) --> [meu, pai], palavras(L).
frase(meupai, L) --> palavras(_), [meu, pai], palavras(L).

frase(sonhei_com, L) --> [eu, sonhei, com], palavras(L).
frase(sonhei_com, L) --> palavras(_), [eu, sonhei, com], palavras(L).

frase(estou_feliz, L) --> [estou, feliz], palavras(L).
frase(estou_feliz, L) --> palavras(_), [estou, feliz], palavras(L).

frase(estou_triste, L) --> [estou, triste], palavras(L).
frase(estou_triste, L) --> palavras(_), [estou, triste], palavras(L).

palavras([]) --> [].
palavras([P|R]) --> [P], palavras(R).

%% Interpretador de linguagem natural
%% Nesta parte, o objetivo é interpretar a entrada do usuário e descobrir com qual regra ela se adequa

interpretar(E,eh_computador) :-sublist(computador,E).

interpretar(E,meu_pai) :- 
    frase(meupai, L, E, []).

interpretar(E,tem_estou_feliz) :- 
    frase(estou_feliz, L, E, []).

interpretar(E,tem_estou_triste) :- 
    frase(estou_triste, L, E, []).

interpretar(E,eh_oi) :- sublist(oi,E).

interpretar(E,tem_nome) :- sublist(nome,E).

interpretar(E,tem_desculpe):- sublist(desculpe,E).

interpretar(E,frase_sair) :- sair(E,[]). 

interpretar(_,frase_nao_reconhecida).

%% teste

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


responder(frase_nao_reconhecida) :-
    write("Não entendi :/ "), nl, nl,
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
    %% delete(LP, '', L). %% para remover os brancos

%% Laço de interação com o usuário 

interagir :-
    prompt(E),
    interpretar(E,Pergunta),
    responder(Pergunta).