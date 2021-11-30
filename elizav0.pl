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


artigo(a) --> [a].
artigo(a) --> [as].
artigo(a) --> [o].
artigo(a) --> [os].

prep(de) --> [de].
prep(de) --> [do].
prep(de) --> [da].
prep(de) --> [para].
prep(de) --> [sem].
prep(de) --> [com].
prep(de) --> [que].

frase(meupai, L) --> palavras(_), [meu, pai], prep(de), palavras(L).
frase(meupai, L) --> palavras(_), [meu, pai], palavras(L).
frase(meupai, L) --> [meu, pai], palavras(L).

frase(sonhei_com, L) --> palavras(_), [eu, sonhei, com], artigo(a), palavras(L).
frase(sonhei_com, L) --> palavras(_), [eu, sonhei, com], palavras(L).
frase(sonhei_com, L) --> [eu, sonhei, com], palavras(L).

frase(eu_sonhei,L) --> palavras(_), [eu, sonhei], palavras(L).
frase(eu_sonhei,L) --> [eu, sonhei], palavras(L).


frase(estou_feliz, L) --> palavras(_), [estou, feliz], palavras(L).
frase(estou_feliz, L) --> [estou, feliz], palavras(L).

frase(estou_triste, L) --> palavras(_), [estou, triste], palavras(L).
frase(estou_triste, L) --> [estou, triste], palavras(L).

frase(sao_como, L1, L2) --> artigo(a), palavras(L1), [sao, como], artigo(a), palavras(L2).
frase(sao_como, L1, L2) --> artigo(a), palavras(L1), [sao, como], palavras(L2).
frase(sao_como, L1, L2) --> palavras(L1), [sao, como], palavras(L2).

frase(eu_lembro, L) --> palavras(_), [eu, lembro], prep(de) , palavras(L).
frase(eu_lembro, L) --> palavras(_), [eu, lembro], palavras(L).
frase(eu_lembro, L) --> [eu, lembro], palavras(L).

palavras([]) --> [].

palavras([P|R]) --> [P], palavras(R).

sublist([P|R], L) :- append([_, [P|R], _], L).
    
sublist(T, L) :- append([_, [T], _], L).

% replace(pronome,novo_pronome,Lista,L)
% replace(minha,sua,[minha,viagem],X)

replace(_, _, [], []).
replace(Pron, NewPron, [Pron|S], [NewPron|SR]) :- replace(Pron, NewPron, S, SR).
replace(Pron, NewPron, [H|S], [H|SR]) :- H \= Pron, replace(Pron, NewPron, S, SR).


replace_minha([C,S],L) :-
    C == minha,
    replace(minha, sua,[C,S],L).

replace_meu([C,S],L) :-
    C == meu,
    replace(meu, seu,[C,S],L).

% member(Item, List) - succeeds if the item is a member of the list;
%   List must be instantiated at the time member is called. Item need not
%   be instantiated.

%% Interpretador de linguagem natural
%% Nesta parte, o objetivo é interpretar a entrada do usuário e descobrir com qual regra ela se adequa


interpretar(E,meu_pai) :- frase(meupai, _, E, []).

interpretar(E,tem_sonhei_com) :- 
    frase(sonhei_com, L, E,[]),
    replace_minha(L,LN),
    asserta(base_respostas(LN,sonhei_com)).

interpretar(E,tem_eu_sonhei) :- 
    frase(eu_sonhei, L, E,[]),
    asserta(base_respostas(L,eu_sonhei)).



interpretar(E,tem_estou_feliz) :- 
    frase(estou_feliz, _, E, []).

interpretar(E,tem_estou_triste) :- 
    frase(estou_triste, _, E, []).

interpretar(E,tem_sao_como) :-
    frase(sao_como, L1, L2,E, []),
    asserta(base_respostas(L1,sao_como1)),
    asserta(base_respostas(L2,sao_como2)).

interpretar(E,tem_eu_lembro) :-
    frase(eu_lembro, L, E, []),
    asserta(base_respostas(L,eu_lembro)).

%% ER de "1 palavra"

interpretar(E,eh_oi) :- sublist(oi,E).

interpretar(E,eh_computador) :-sublist(computador,E).

interpretar(E,tem_nome) :- sublist(nome,E).

interpretar(E,tem_desculpe):- sublist(desculpe,E).

interpretar(E,tem_sonho) :- sublist(sonho,E). 

interpretar(E,tem_parece) :- sublist(parece,E). 

interpretar(E,tem_mesmo) :- sublist(mesmo,E). 

interpretar(E,tem_sou) :- sublist(sou,E). 

interpretar(E,tem_sim) :- sublist(sim,E). 

interpretar(E,tem_nao) :- sublist(nao,E). 

interpretar(E,tem_alguem) :- sublist(alguem,E). 

interpretar(E,tem_todos) :- sublist(todos,E). 

interpretar(E,tem_sempre) :- sublist(sempre,E). 

interpretar(E,tem_talvez) :- sublist(talvez,E). 

interpretar(E,frase_sair) :- sair(E,[]). 

interpretar(_,frase_nao_reconhecida).

%% teste

responder(meu_pai) :-
    random_member(Resp, ['Seu pai?',
                        'Ele influencia você fortemente?',
                        'O que mais vem à mente quando você pensa no seu pai?']),
    enumere(Resp),
    interagir. 


responder(tem_sonhei_com) :-
    call(base_respostas,Resp,sonhei_com),
    write("Como você se sente em relação a "), enum_resp(Resp), write(" na verdade?"), nl,nl,
    retractall(base_respostas(_,_)),
    interagir.

responder(tem_eu_sonhei) :-
    call(base_respostas,Res,eu_sonhei),
    random_member(Resp, ['Realmente?' + write(Res) +'?',
                        'Ele influencia você fortemente?',
                        'O que mais vem à mente quando você pensa no seu pai?']),
    enumere(Resp),
    retractall(base_respostas(_,_)),
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

responder(tem_sao_como) :-
    call(base_respostas,Resp1,sao_como1),
    call(base_respostas,Resp2,sao_como2),
   %% enumere(L),
    write("Que semelhança você vê entre " ), enum_resp(Resp1), write(' e '), enum_resp(Resp2), nl,nl,
    retractall(base_respostas(_,_)),
    interagir.


responder(tem_eu_lembro) :-
    call(base_respostas,Res,eu_lembro),
    random_member(Resp, ['Você normalmente lembra' + write(Res) +'?' ,
                'Lembrar'+ write(Res) + 'traz alguma outra lembrança à sua mente?',
                'Que outras coisas você lembra?']),
    enumere(Resp),
    retractall(base_respostas(_,_)),
    interagir.

responder(eh_oi) :-
    write("Como vai você? Por favor, me fale do seu problema."),nl,nl,
    interagir.

responder(eh_computador) :-
    random_member(Resp, ['Computadores te incomodam?',
                'O que você acha sobre máquinas?',
                'Porque você menciona computadores?',
                'O que você acha que máquinas tem a ver com o seu problema?']),
    enumere(Resp),
    interagir.

responder(tem_nome) :-
    write("Não estou interessado em nomes"),nl,nl,
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

responder(tem_sim) :-
    random_member(Resp, ['Você parece uma pessoa bem positiva',
                'Tem certeza?', 'Entendo']),
    enumere(Resp),
    interagir.

responder(tem_nao) :-
    random_member(Resp, ['Porque não?',
                'Você está sendo um pouco negativo', 'Você diz não só pra ser negativo?']),
    enumere(Resp),
    interagir.

responder(tem_alguem) :-
    write("Você pode ser mais específico?"),nl,nl,
    interagir.

responder(tem_todos) :-
    random_member(Resp, ['Com certeza não todos',
                        'Pode pensar em alguém em particular?',
                        'Quem por exemplo?',
                        'Você está pensando em alguém em particular?']),
    enumere(Resp),
    interagir.

responder(tem_sempre) :-
    random_member(Resp, ['Você pode dar um exemplo específico',
                        'Quando?',
                        'Sobre o que você está pensando?',
                        'Realmente sempre?']),
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

responder(tem_talvez) :-
    write("Você não parece muito certo"),nl,nl,
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
    write('  '), enum_resp(L), nl,nl.


enum_resp([]) :- write(" * ").

enum_resp([P]) :- write(P).

enum_resp([P1,P2]) :- 
    write(P1), write(" "),  write(P2).

enum_resp(P) :- 
    write(P).


enum_resp([_,P2]) :- write(P2).

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