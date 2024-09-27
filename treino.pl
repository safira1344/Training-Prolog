%pode adicionar e remover fatos em tempo de execução coloca o fato e a raridade
:- dynamic mulher/1.
:- dynamic homem/1.
:- dynamic genitor/2.
:- dynamic animal/1.

%assert/1 - acrescenta o fato/regra como último item do predicado

%asserta/1 - acrescenta o fato/regra como primeiro item do predicado.

%retract/1 - remove da base de conhecimento a primeira cláusula (fato ou regras) que unifica com o termo que é passado como parâmetro.

%retractall/1 - remove da base de conhecimento todos os fatos ou regras cuja cláusula (fato ou regra) unifique com o termo que é passado como parâmetro;

%abolish/1 - remove da base de conhecimento todos os fatos e regras pelo nome da regra ou fato/raridade que é passada como parametro (sao removidos predicados estaticos também).

%abolish/2 - semelhante a abolish/1, mas passando o nome do fato/regra e a sua raridade separadamente (sao removidos predicados estaticos tambem).

%listing(). - retorna todos os fatos corespondentes, ou todas as variáveis ou todos os átomos


start() :- write('Digite um valor de X:'),nl,
           read(X),nl,
           write(X),nl.

            %raridade 1
            mulher(pam).
            mulher(ann).
            mulher(liz).
            mulher(pat).
            %toda essa linha se chama de predicado.
            homem(tom).
            homem(bob).
            homem(jim).

            %raridade 2
            % genitor(pam,bob).
            % genitor(tom,bob).
            % genitor(tom,liz).
            % genitor(bob,ann).
            % prole(X,Y) :- genitor(Y,X).
            % mae(X,Y) :- mulher(X), genitor(X,Y).
            % avos(X,Z) :- genitor(X,Y), genitor(Y,Z).

            animal(urso).
            animal(peixe).
            animal(peixinho).
            animal(lince).
            animal(raposa).
            animal(coelho).
            animal(guaxinim).
            animal(veado).

            planta(alga).
            planta(grama).

            come(urso,peixe).
            come(lince,veado).
            come(urso,raposa).
            come(urso,veado).
            come(peixe,peixinho).
            come(peixinho,alga).
            come(guaxinim,peixe).
            come(raposa,coelho).
            come(coelho,grama).
            come(veado,grama).
            come(urso,guaxinim).  

            presa(X) :- come(_, X), animal(X). 


            estados(mg,'Belo Horizonte').
            estados(sp,'São Paulo').
            estados(rj, 'Rio de Janeiro').
            estados(es,'Vitoria').
            capital(Estado,Capital) :- estados(Estado,Capital).


            doa(a,a).
            doa(a,ab).
            doa(b,b).
            doa(b,ab).
            doa(ab,ab).
            doa(o,a).
            doa(o,b).
            doa(o,ab).
            doa(o,o).

            recebe(a,a).
            recebe(a,o).
            recebe(b,b).
            recebe(b,o).
            recebe(ab,a).
            recebe(ab,ab).
            recebe(ab,o).
            recebe(o,o).


            soma(A,B,S) :- S is A + B.

%Crie uma regra em Prolog que peça no console um número inteiro e imprima na tela se o número é maior que 100 ou se é menor ou igual a 100.

            maiorQue100() :- write('Escreva um numero:'),
                            read(X),
                            (   
                                (X>100, write('O numero e maior que 100'));
                                (X=:=100, write('O numero e igual 100'));
                                (X<100, write('O numero e menor que 100'))
                            ).
                
%Suponha os seguintes fatos e escreva uma regra para identificar a situação de um determinado aluno.

            % nota(joao,5.0).
            % nota(mariana,9.0).
            % nota(joaquim,4.5).
            % nota(maria,6.0).
            % nota(cleuza,8.5).
            % nota(mara,4.0).
            % nota(joana, 8.0).
            % nota(jose, 6.5).
            % nota(mary,10.0).
            % nota(nathalia,20.0).

            % diario(X) :- nota(X,Nota), 
            %                         (
            %                             Nota>=7, Nota<10,write('Aprovado!');
            %                             Nota >= 5, Nota < 6.9, write('Recuperacao!');
            %                             Nota>0, Nota < 6.9, write('reprovado!')
                                    % ).
                                        
        

            imc(Peso,Altura) :- X is Peso/(Altura*Altura),
                                write('Seu IMC e: '), write(X).

            
            %como descobrir se dois argumentos são descendentes até o nível de bisavô
            descendente(X,Y) :- genitor(Y,X).
            descendente(X,Y) :- genitor(Y,Z), genitor(Z,X).
            descendente(X,Y) :- genitor(Y,Z), genitor(Z,W), genitor(W,X).


            %a medida que aumentamos o nivel da arvore genealogica precisamos criar novas linhas, fazendo essas consultas do genitor até veriificar se é descendente, pra evitar essa repetição de linhas usamos a recursão pra resolver esse problema.
            descendente(X,Y) :- genitor(Y,X).
            descendente(X,Y) :- genitor(Y,W), descendente(X,W).

            %fatorial se não tivesse recursão seria assim
            % fatorial(5) = fatorial(4) * 5
            %               (fatorial(3)*4)*5
            %               ((fatorial(2)*3)*4)*5
            %               (((fatorial(1)*2)*3)*4)*5
            %               ((((fatorial(0)))))

            %caso base
            fatorial(0,1).
            %caso recursivo e geral
            fatorial(N,F) :- N > 0,
                             N1 is N - 1,
                             F1 is N * F.
                             fatorial(N1,F1).



            pertence-cadeia(X,Y) :- animal(X), come(Y,X).
            pertence-cadeia(X,Y) :- come(Y,Z), pertence-cadeia(X,Z).

            %caso base: a soma de uma lista vazia é 0
            soma_lista([],0).
            %caso recursivo: a soma de uma lista [Cabeca|cauda] e cabeca + soma(cauda)
            soma_lista([Cabeca | Cauda], Soma) :- soma_lista(Cauda, SomaCauda),
                                                  Soma is Cabeca + SomaCauda.
                                                

            %caso base: o tamanho de uma lista vazia é 0
            tamanho_lista([],0).
            %caso recursivo: o tamanho de uma lista [Cabeça | Cauda] é 1 + tamanho da Cauda
            tamanho_lista([_ | Cauda], Tamanho) :- tamanho_lista(Cauda, TamanhoCauda),
                                                        Tamanho is TamanhoCauda + 1.

            %Soma dos números de N até 1
            %caso base
            soma_n(0,0).
            %caso recursivo
            soma_n(N, S) :-
                            N > 0,
                            N1 is N - 1,
                            soma_n(N1, S1),
                            S is S1 + N.
                                
            

            %lista de exercicios prolog
            %1 questao
            nota(joao,5.0).
            nota(maria,6.0).
            nota(joana,8.0).
            nota(mariana,9.0).
            nota(cleuza,8.5).
            nota(jose,6.5).
            nota(jaoquim,4.5).
            nota(mara,4.0).
            nota(mary,10.0).
            nota(rafael,8.0).
            nota(ana,9.2).
            nota(inocencio,5.0).

            %cuidado com os argumentos do fato são letras maiúsculas se for variável
            % diario(X) :- nota(X,Nota),
            %                             (
            %                             (Nota >= 7.0, Nota =< 10.0,write('Aprovado'));
            %                             (Nota >= 5.0, Nota =< 6.9, write('Recuperacao'));
            %                             (Nota >= 0.0, Nota =< 4.9, write('Reprovado'))
            %                             ).

            %ou

            situacao(Aluno, aprovado) :- nota(Aluno, Nota),
                                         Nota >= 7.0.

            situacao(Aluno, recuperacao) :- nota(Aluno, Nota),
                                           Nota >= 5.0,
                                           Nota =< 6.9.

            situacao(Aluno, reprovado) :- nota(Aluno, Nota),
                                          Nota >= 0.0,
                                          Nota =< 4.9.


            %2 questao
            %no caso base quando chegar na lista vazia só retorna ela vazia por se tratar de átomos de palavras
            aprovados([],[]).

            aprovados([Aluno | RestoAlunos], [Aluno|RestoAprovados]) :- situacao(Aluno, aprovado),
            aprovados(RestoAlunos, RestoAprovados).

            aprovados([ _ | Resto], Aprovados) :- aprovados(Resto, Aprovados).

            
    %3 questão
    % Fatos: definindo relações básicas

    % Relações de paternidade e maternidade
    pai(jose, joao).
    pai(joao, pedro).
    pai(pedro, carlos).

    mae(maria, joao).
    mae(ana, pedro).
    mae(carla, carlos).

    % Definindo sexo das pessoas para ajudar nas regras
    homem(jose).
    homem(joao).
    homem(pedro).
    homem(carlos).

    mulher(maria).
    mulher(ana).
    mulher(carla).

    mae(X,Y) :- mae(X,Y).
    pai(X,Y) :- pai(X,Y).

    filho(X,Y) :- (pai(Y,X); mae(Y,X), homem(X)).
    filha(X,Y) :- (pai(Y,X); mae(Y,X), mulher(X)).

    irmao(X,Y) :- (pai(P,X),pai(P,Y); 
                   mae(M,X),mae(M,Y),
                   X \= Y, homem(X)).
    irma(X,Y) :- (pai(P,X), pai(P,Y);
                  mae(M,X), mae(M,Y),
                  X \= Y, mulher(X)).

    avo_homem(X,Y) :- pai(X,A),pai(A,Y); mae(A,Y), homem(X).
    avo_mulher(X,Y) :- mae(X,A), mae(A,Y); pai(A,Y), mulher(X). 

    tio(X,Y) :- irmao(X,Z), pai(Z,Y); mae(Z,Y).
    tia(X,Y) :- irma(X,Z), pai(Z,Y); mae(Z,Y).

    cunhado(X, Y) :- casado(X, Z), (irmao(Z, Y) ; irma(Z, Y)), homem(X).
    cunhada(X, Y) :- casado(X, Z), (irmao(Z, Y) ; irma(Z, Y)), mulher(X).

    genro(X, Y) :- casado(X, Z), (pai(Y, Z) ; mae(Y, Z)), homem(X).
    nora(X, Y) :- casado(X, Z), (pai(Y, Z) ; mae(Y, Z)), mulher(X).

    primo(X, Y) :- (tio(Z, X), pai(Z, Y) ; tia(Z, X), mae(Z, Y)), homem(X).
    prima(X, Y) :- (tio(Z, X), pai(Z, Y) ; tia(Z, X), mae(Z, Y)), mulher(X).

    bisneto(X, Y) :- (avo(Y, Z) ; avoa(Y, Z)), (pai(Z, X) ; mae(Z, X)), homem(X).
    bisneta(X, Y) :- (avo(Y, Z) ; avoa(Y, Z)), (pai(Z, X) ; mae(Z, X)), mulher(X).


    %4 questao
    herbivoro(X) :- come(X, Y), planta(Y).

    carnivoro(X) :- come(X,Y), animal(Y).

    %5 questao
    % Fatos: relação de amizade entre pessoas
    amigo(ana, beatriz).
    amigo(beatriz, ana).
    amigo(beatriz, carlos).
    amigo(carlos, ana).
    amigo(ana, daniel).
    amigo(daniel, elisa).
    amigo(elisa, beatriz).
    amigo(beatriz, daniel).

    amigos_reciprocos(X,Y) :- amigo(X,Y), amigo(Y,X).
    amigos_em_comum(X,Y,Z) :- amigo(X,Z), amigo(Y,Z).

    %6 questao

    par([]).
    par([_,_ | Cauda]) :- par(Cauda).

    impar([_]).
    impar([_,_ | Cauda]) :- impar(Cauda).

    %ou

    % contador([],0).
    % contador([_|Cauda], Soma) :- contador(Cauda,Resultado),
    %                               Soma is Resultado + 1.      

    % par(Lista) :- contador(Lista, N),
    %               (0 is N mod 2).

    % impar(Lista) :- \+ par(Lista).             


    %7 questao
    inverso(Lista, ListaInvertida) :- inverso_aux(Lista,[],ListaInvertida).

    inverso_aux([], Acumulador, Acumulador).
    inverso_aux([Cabeca | Cauda], Acumulador, ListaInvertida) :- inverso_aux(Cauda, [Cabeca | Acumulador], ListaInvertida).

    %8 questao

    % Caso base: quando a lista tem exatamente dois elementos, o primeiro é o penúltimo
    penultimo([Penultimo, _], Penultimo).
    penultimo([_ | Cauda], Penultimo) :- penultimo(Cauda, Penultimo).

    ultimo([_, Ultimo], Ultimo).
    ultimo([_|Cauda],Ultimo) :- ultimo(Cauda,Ultimo).

    primeiro([Cabeca|_], Cabeca).