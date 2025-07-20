% N 106559 Francisco Nascimento
:- set_prolog_flag(answer_write_options,[max_depth(0)]). 
:- [dados], [keywords].

%-------------------------------------------------------3.1--------------------------------------------------------

/* 
eventosSemSalas(EventosSemSala) vai encontrar e retornar todos os IDs dos eventos
que nao tem sala em uma lista organizada.
*/
eventosSemSalas(EventosSemSala) :-
    findall(ID, evento(ID,_,_,_,semSala), EventosDesorg),
    sort(EventosDesorg, EventosSemSala).

/*
eventosSemSalasDiaSemana(DiaDaSemana, EventosSemSala) vai encontrar todos os IDs dos eventos
que nao tem sala e que ocorrem no dia da semana dado e retorna-os em uma lista organizada.
*/
eventosSemSalasDiaSemana(DiaDaSemana, EventosSemSala) :-
    findall(ID, (horario(ID,DiaDaSemana,_,_,_,_), evento(ID,_,_,_,semSala)), EventosDesorg),
    sort(EventosDesorg, EventosSemSala).

/*
periodo(ListaPeriodos, NovaLista) serve para auxiliar as outras funcoes com o objetivo de
resolver o problema das disciplinas semestrais. Esta vai adicionar a ListaPeriodos o 
semestre ao periodo existente e retorna uma nova lista com tudo junto.
*/
periodoAux(El, [El, p1_2]):-
    (El = p1; El = p2).
periodoAux(El, [El, p3_4]):-
    (El = p3; El = p4).

periodo(ListaPeriodos, NovaLista) :-
    maplist(periodoAux(), ListaPeriodos, NovaLista1),
    flatten(NovaLista1, NovaLista).

/*
eventosSemSalasPeriodo(ListaPeriodos, EventosSemSala) vai procurar todos os IDs de eventos
que nao tem sala e que ocorrem em num periodo especificado, contando com as disciplinas
semestrais, e retorna uma lista ordenada com esses IDs.
*/
eventosSemSalasPeriodo(ListaPeriodos, EventosSemSala) :-
    periodo(ListaPeriodos, NovaLista),
    findall(ID, (evento(ID,_,_,_,semSala), member(Membro, NovaLista), horario(ID,_,_,_,_,Membro)), EventosDesorg),
    sort(EventosDesorg, EventosSemSala).

%-------------------------------------------------------3.2--------------------------------------------------------

/*
organizaEventos(ListaEventos, Periodo, EventosNoPeriodo) utiliza recursao para retirar 
de uma lista de IDs de eventos, os IDs que vao de acordo com o periodo especificado e
retorna uma lista organizada desses IDs.
*/
organizaEventosAux([], _, []).
organizaEventosAux([ID|OutrosIDs], Periodo, [ID|IDsNoPeriodo]) :-
    horario(ID, _, _, _, _, Periodo), !,
    organizaEventosAux(OutrosIDs, Periodo, IDsNoPeriodo).
organizaEventosAux([_|OutrosIDs], Periodo, IDsNoPeriodo) :-
    organizaEventosAux(OutrosIDs, Periodo, IDsNoPeriodo).

organizaEventos(ListaEventos, Periodo, EventosNoPeriodo) :-
    organizaEventosAux(ListaEventos, Periodo, EventosNoPeriodo1),
    (Periodo == p1; Periodo == p2),
    organizaEventosAux(ListaEventos, p1_2, EventosNoPeriodo2),
    append(EventosNoPeriodo1, EventosNoPeriodo2, EventosNoPeriodoDesorg),
    sort(EventosNoPeriodoDesorg, EventosNoPeriodo).

organizaEventos(ListaEventos, Periodo, EventosNoPeriodo) :-
    organizaEventosAux(ListaEventos, Periodo, EventosNoPeriodo1),
    (Periodo == p3; Periodo == p4),
    organizaEventosAux(ListaEventos, p3_4, EventosNoPeriodo2),
    append(EventosNoPeriodo1, EventosNoPeriodo2, EventosNoPeriodoDesorg),
    sort(EventosNoPeriodoDesorg, EventosNoPeriodo).

/*
eventosMenoresQue(Duracao, ListaEventosMenoresQue) serve para encontrar todos os IDs de 
eventos que tem uma duracao menor ou igual ao valor especificado em Duracao e retorna 
esses valores organizados em um lista.
*/
eventosMenoresQue(Duracao, ListaEventosMenoresQue) :-
    findall(ID, (horario(ID, _, _, _, D, _), D =< Duracao), ListaIds),
    sort(ListaIds, ListaEventosMenoresQue).

/*
eventosMenoresQueBool(ID, Duracao) verifica se o evento com o ID especificado tem uma
duracao menor ou igual ao valor especificado em Duracao. Ela retorna true se a duracao
do evento e menor ou igual a Duracao, e false caso contrario.
*/
eventosMenoresQueBool(ID, Duracao) :-
    horario(ID, _, _, _, D, _),
    D =< Duracao.

/*
procuraDisciplinas(Curso, ListaDisciplinas) vai encontrar todas as disciplinas associadas
ao curso especificado em Curso e retornar uma Lista organizada dessas disciplinas.
*/
procuraDisciplinas(Curso, ListaDisciplinas) :-
    findall(Disciplina, (turno(ID, Curso, _, _), evento(ID, Disciplina, _, _, _)), ListaDisciplinasDesorg),
    sort(ListaDisciplinasDesorg, ListaDisciplinas).

/*
organizaDisciplinas(ListaDisciplinas, Curso, Semestres), atraves da recursao, vai percorrer
a ListaDisciplinas e verificar se as disciplinas de um curso especificado pertecem ao 1
semestre ou ao 2 semestre, contando com as disciplinas semestrais. Assim, esta funcao
vai retornar uma lista com 2 listas onde a primeira vai possuir as disciplinas do 1 semestre
e a segunda do 2 semestre.
*/
organizaDisciplinas([], _, [[],[]]):- !.
organizaDisciplinas([Disciplina|RestoDisciplina], Curso,  [[Disciplina | Semestre1], Semestre2]):-
    evento(ID, Disciplina, _, _, _),
    turno(ID, Curso, _, _),
    horario(ID, _, _, _, _, Periodo),
    member(Periodo, [p1_2, p1, p2]), !,
    organizaDisciplinas(RestoDisciplina, Curso, [Semestre1, Semestre2]).
organizaDisciplinas([Disciplina|RestoDisciplina], Curso,  [Semestre1, [Disciplina | Semestre2]]):-
    evento(ID, Disciplina, _, _, _),
    turno(ID, Curso, _, _),
    horario(ID, _, _, _, _, Periodo),
    member(Periodo, [p3_4, p3, p4]), !,
    organizaDisciplinas(RestoDisciplina, Curso, [Semestre1, Semestre2]).

/*
horasCurso(Periodo, Curso, Ano, TotalHoras) vai calcular e retornar o total de horas de
um determinado curso, em um determinado ano e periodo.
*/
horasCurso(Periodo, Curso, Ano, TotalHoras) :-
    periodo([Periodo], ListaPeriodos),
    findall(ID, (turno(ID, Curso, Ano, _)), ListaIDs),
    sort(ListaIDs, ListaIDsOrg),
    findall(Duracao,(member(ID, ListaIDsOrg), member(P, ListaPeriodos), horario(ID, _, _, _, Duracao, P)), ListaHoras),
    sumlist(ListaHoras, TotalHoras).

/*
evolucaoHorasCurso(Curso, Evolucao) vai utilizar as horasCurso para calcular o total de
horas de um determinado curso e retornar um lista organizada de tuplos com anos, periodos
e os numeros de horas correspondentes a esses anos e periodos.
*/
evolucaoHorasCurso(Curso, Evolucao) :-
    findall((Ano, Periodo, NumHoras), (member(Periodo, [p1, p2, p3, p4]), member(Ano, [1, 2, 3]), 
            horasCurso(Periodo, Curso, Ano, NumHoras)), EvolucaoDesorg),
    sort(EvolucaoDesorg, Evolucao).

%-------------------------------------------------------3.3--------------------------------------------------------
/*
ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas) vai 
calcular e retornar as horas de um evento que ocupa um determinado slot de tempo. O slot de
tempo e especificado pelas horas de inicio e fim dadas.
*/
ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas) :-
    HoraInicioDada < HoraFimEvento,
    HoraFimDada > HoraInicioEvento,
    Horas is min(HoraFimDada, HoraFimEvento) - max(HoraInicioDada, HoraInicioEvento).

/*
numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras) vai
calcular e retornar o numero total de horas ocupadas por eventos em um determinado 
periodo, tipo de sala, dia da semana, e slot de tempo especificados ou calculados
atraves do ocupaSlot.
*/
numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras) :-
    salas(TipoSala, Salas),
    periodo([Periodo], ListaPeriodos),
    findall(Duracao, (member(Sala, Salas), member(P, ListaPeriodos), 
            evento(ID,_,_,_,Sala), horario(ID, DiaSemana, Inicio, Fim, _, P), 
            ocupaSlot(HoraInicio, HoraFim, Inicio, Fim, Duracao)), ListaHoras),
    sumlist(ListaHoras, SomaHoras).
/*
ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max) vai calcular e retornar a ocupacao
maxima possivel de um determinado tipo de sala em um dado slot de tempo especificado
pelas horas de inicio e fim.
*/
ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max) :-
    salas(TipoSala, Salas),
    length(Salas, NumSalas),
    Max is (HoraFim - HoraInicio) * NumSalas.

/*
percentagem(SomaHoras, Max, Percentagem) vai calcular e retornar a percentagem atraves
de uma formula fornecida.
*/
percentagem(SomaHoras, Max, Percentagem):-
    Percentagem is (SomaHoras / Max) * 100.

/*
ocupacaoCritica(HoraInicio, HoraFim, Threshold, Resultados) vai encontrar situacoes criticas
de ocupacao em um determinado slot de tempo especificado pelas horas de inicio e fim. Esta vai
retornar uma lista ordenada de tuplos com Dias de semana, tipos de sala e percentagens 
arredondadas de ocupcao correspondestes a esses dias de semana e tipos de sala.
*/
ocupacaoCritica(HoraInicio, HoraFim, Threshold, Resultados) :- 
    findall(casosCriticos(DiaSemana, TipoSala, PercentagemArredondada),
            (member(DiaSemana,[segunda-feira, terca-feira, quarta-feira, quinta-feira, sexta-feira]),
             member(Periodo, [p1, p2, p3, p4]),
             salas(TipoSala, _),
             numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras),
             ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max),
             percentagem(SomaHoras, Max, Percentagem),
             Percentagem > Threshold,
             ceiling(Percentagem, PercentagemArredondada)), 
             Resultados1),
    sort(Resultados1, Resultados).
%-------------------------------------------------------3.4--------------------------------------------------------

%Restricoes possiveis:

cab1(Pessoa4,[[_,_,_], [Pessoa4,_], [_,_,_]]).
cab2(Pessoa5,[[_,_,_], [_,Pessoa5], [_,_,_]]).

honra(Pessoa5, Pessoa3, [[_,_,Pessoa3], [_,Pessoa5], [_,_,_]]).
honra(Pessoa4, Pessoa6, [[_,_,_], [Pessoa4,_], [Pessoa6,_,_]]).
lado(Pessoa1, Pessoa2, [[Pessoa1,Pessoa2,_], [_,_], [_,_,_]]).
lado(Pessoa2, Pessoa3, [[_,Pessoa2,Pessoa3], [_,_], [_,_,_]]).
lado(Pessoa6, Pessoa7, [[_,_,_], [_,_], [Pessoa6,Pessoa7,_]]).
lado(Pessoa7, Pessoa8, [[_,_,_], [_,_], [_,Pessoa7,Pessoa8]]).
lado(Pessoa2, Pessoa1, [[Pessoa1, Pessoa2,_], [_,_], [_,_,_]]).
lado(Pessoa3, Pessoa2, [[_,Pessoa2,Pessoa3], [_,_], [_,_,_]]).
lado(Pessoa7, Pessoa6, [[_,_,_], [_,_], [Pessoa6,Pessoa7,_]]).
lado(Pessoa8, Pessoa7, [[_,_,_], [_,_], [_,Pessoa7,Pessoa8]]).

naoLado(Pessoa1, Pessoa3, [[Pessoa1,_,Pessoa3], [_,_], [_,_,_]]).
naoLado(Pessoa1, Pessoa4, [[Pessoa1,_,_], [Pessoa4,_], [_,_,_]]).
naoLado(Pessoa1, Pessoa5, [[Pessoa1,_,_], [_,Pessoa5], [_,_,_]]).
naoLado(Pessoa1, Pessoa6, [[Pessoa1,_,_], [_,_], [Pessoa6,_,_]]).
naoLado(Pessoa1, Pessoa7, [[Pessoa1,_,_], [_,_], [_,Pessoa7,_]]).
naoLado(Pessoa1, Pessoa8, [[Pessoa1,_,_], [_,_], [_,_,Pessoa8]]).
naoLado(Pessoa2, Pessoa4, [[_,Pessoa2,_], [Pessoa4,_], [_,_,_]]).
naoLado(Pessoa2, Pessoa5, [[_,Pessoa2,_], [_,Pessoa5], [_,_,_]]).
naoLado(Pessoa2, Pessoa6, [[_,Pessoa2,_], [_,_], [Pessoa6,_,_]]).
naoLado(Pessoa2, Pessoa7, [[_,Pessoa2,_], [_,_], [_,Pessoa7,_]]).
naoLado(Pessoa2, Pessoa8, [[_,Pessoa2,_], [_,_], [_,_,Pessoa8]]).
naoLado(Pessoa3, Pessoa1, [[Pessoa1,_,Pessoa3], [_,_], [_,_,_]]).
naoLado(Pessoa3, Pessoa4, [[_,_,Pessoa3], [Pessoa4,_], [_,_,_]]).
naoLado(Pessoa3, Pessoa5, [[_,_,Pessoa3], [_,Pessoa5], [_,_,_]]).
naoLado(Pessoa3, Pessoa6, [[_,_,Pessoa3], [_,_], [Pessoa6,_,_]]).
naoLado(Pessoa3, Pessoa7, [[_,_,Pessoa3], [_,_], [_,Pessoa7,_]]).
naoLado(Pessoa3, Pessoa8, [[_,_,Pessoa3], [_,_], [_,_,Pessoa8]]).
naoLado(Pessoa4, Pessoa1, [[Pessoa1,_,_], [Pessoa4,_], [_,_,_]]).
naoLado(Pessoa4, Pessoa2, [[_,Pessoa2,_], [Pessoa4,_], [_,_,_]]).
naoLado(Pessoa4, Pessoa3, [[_,_,Pessoa3], [Pessoa4,_], [_,_,_]]).
naoLado(Pessoa4, Pessoa5, [[_,_,_], [Pessoa4,Pessoa5], [_,_,_]]).
naoLado(Pessoa4, Pessoa6, [[_,_,_], [Pessoa4,_], [Pessoa6,_,_]]).
naoLado(Pessoa4, Pessoa7, [[_,_,_], [Pessoa4,_], [_,Pessoa7,_]]).
naoLado(Pessoa4, Pessoa8, [[_,_,_], [Pessoa4,_], [_,_,Pessoa8]]).
naoLado(Pessoa5, Pessoa1, [[Pessoa1,_,_], [_,Pessoa5], [_,_,_]]).
naoLado(Pessoa5, Pessoa2, [[_,Pessoa2,_], [_,Pessoa5], [_,_,_]]).
naoLado(Pessoa5, Pessoa3, [[_,_,Pessoa3], [_,Pessoa5], [_,_,_]]).
naoLado(Pessoa5, Pessoa4, [[_,_,_], [Pessoa4,Pessoa5], [_,_,_]]).
naoLado(Pessoa5, Pessoa6, [[_,_,_], [_,Pessoa5], [Pessoa6,_,_]]).
naoLado(Pessoa5, Pessoa7, [[_,_,_], [_,Pessoa5], [_,Pessoa7,_]]).
naoLado(Pessoa5, Pessoa8, [[_,_,_], [_,Pessoa5], [_,_,Pessoa8]]).
naoLado(Pessoa6, Pessoa1, [[Pessoa1,_,_], [_,_], [Pessoa6,_,_]]).
naoLado(Pessoa6, Pessoa2, [[_,Pessoa2,_], [_,_], [Pessoa6,_,_]]).
naoLado(Pessoa6, Pessoa3, [[_,_,Pessoa3], [_,_], [Pessoa6,_,_]]).
naoLado(Pessoa6, Pessoa4, [[_,_,_], [Pessoa4,_], [Pessoa6,_,_]]).
naoLado(Pessoa6, Pessoa5, [[_,_,_], [_,Pessoa5], [Pessoa6,_,_]]).
naoLado(Pessoa6, Pessoa8, [[_,_,_], [_,_], [Pessoa6,_,Pessoa8]]).
naoLado(Pessoa7, Pessoa1, [[Pessoa1,_,_], [_,_], [_,Pessoa7,_]]).
naoLado(Pessoa7, Pessoa2, [[_,Pessoa2,_], [_,_], [_,Pessoa7,_]]).
naoLado(Pessoa7, Pessoa3, [[_,_,Pessoa3], [_,_], [_,Pessoa7,_]]).
naoLado(Pessoa7, Pessoa4, [[_,_,_], [Pessoa4,_], [_,Pessoa7,_]]).
naoLado(Pessoa7, Pessoa5, [[_,_,_], [_,Pessoa5], [_,Pessoa7,_]]).
naoLado(Pessoa8, Pessoa1, [[Pessoa1,_,_], [_,_], [_,_,Pessoa8]]).
naoLado(Pessoa8, Pessoa2, [[_,Pessoa2,_], [_,_], [_,_,Pessoa8]]).
naoLado(Pessoa8, Pessoa3, [[_,_,Pessoa3], [_,_], [_,_,Pessoa8]]).
naoLado(Pessoa8, Pessoa4, [[_,_,_], [Pessoa4,_], [_,_,Pessoa8]]).
naoLado(Pessoa8, Pessoa5, [[_,_,_], [_,Pessoa5], [_,_,Pessoa8]]).
naoLado(Pessoa8, Pessoa6, [[_,_,_], [_,_], [Pessoa6,_,Pessoa8]]).

frente(Pessoa1, Pessoa6, [[Pessoa1,_,_], [_,_], [Pessoa6,_,_]]).
frente(Pessoa2, Pessoa7, [[_,Pessoa2,_], [_,_], [_,Pessoa7,_]]).
frente(Pessoa3, Pessoa8, [[_,_,Pessoa3], [_,_], [_,_,Pessoa8]]).
frente(Pessoa6, Pessoa1, [[Pessoa1,_,_], [_,_], [Pessoa6,_,_]]).
frente(Pessoa7, Pessoa2, [[_,Pessoa2,_], [_,_], [_,Pessoa7,_]]).
frente(Pessoa8, Pessoa3, [[_,_,Pessoa3], [_,_], [_,_,Pessoa8]]).

naoFrente(Pessoa1, Pessoa2, [[Pessoa1,Pessoa2,_], [_,_], [_,_,_]]).
naoFrente(Pessoa1, Pessoa3, [[Pessoa1,_,Pessoa3], [_,_], [_,_,_]]).
naoFrente(Pessoa1, Pessoa4, [[Pessoa1,_,_], [Pessoa4,_], [_,_,_]]).
naoFrente(Pessoa1, Pessoa5, [[Pessoa1,_,_], [_,Pessoa5], [_,_,_]]).
naoFrente(Pessoa1, Pessoa7, [[Pessoa1,_,_], [_,_], [_,Pessoa7,_]]).
naoFrente(Pessoa1, Pessoa8, [[Pessoa1,_,_], [_,_], [_,_,Pessoa8]]).
naoFrente(Pessoa2, Pessoa1, [[Pessoa1,Pessoa2,_], [_,_], [_,_,_]]).
naoFrente(Pessoa2, Pessoa3, [[_,Pessoa2,Pessoa3], [_,_], [_,_,_]]).
naoFrente(Pessoa2, Pessoa4, [[_,Pessoa2,_], [Pessoa4,_], [_,_,_]]).
naoFrente(Pessoa2, Pessoa5, [[_,Pessoa2,_], [_,Pessoa5], [_,_,_]]).
naoFrente(Pessoa2, Pessoa6, [[_,Pessoa2,_], [_,_], [Pessoa6,_,_]]).
naoFrente(Pessoa2, Pessoa8, [[_,Pessoa2,_], [_,_], [_,_,Pessoa8]]).
naoFrente(Pessoa3, Pessoa1, [[Pessoa1,_,Pessoa3], [_,_], [_,_,_]]).
naoFrente(Pessoa3, Pessoa2, [[_,Pessoa2,Pessoa3], [_,_], [_,_,_]]).
naoFrente(Pessoa3, Pessoa4, [[_,_,Pessoa3], [Pessoa4,_], [_,_,_]]).
naoFrente(Pessoa3, Pessoa5, [[_,_,Pessoa3], [_,Pessoa5], [_,_,_]]).
naoFrente(Pessoa3, Pessoa6, [[_,_,Pessoa3], [_,_], [Pessoa6,_,_]]).
naoFrente(Pessoa3, Pessoa7, [[_,_,Pessoa3], [_,_], [_,Pessoa7,_]]).
naoFrente(Pessoa4, Pessoa1, [[Pessoa1,_,_], [Pessoa4,_], [_,_,_]]).
naoFrente(Pessoa4, Pessoa2, [[_,Pessoa2,_], [Pessoa4,_], [_,_,_]]).
naoFrente(Pessoa4, Pessoa3, [[_,_,Pessoa3], [Pessoa4,_], [_,_,_]]).
naoFrente(Pessoa4, Pessoa5, [[_,_,_], [Pessoa4,Pessoa5], [_,_,_]]).
naoFrente(Pessoa4, Pessoa6, [[_,_,_], [Pessoa4,_], [Pessoa6,_,_]]).
naoFrente(Pessoa4, Pessoa7, [[_,_,_], [Pessoa4,_], [_,Pessoa7,_]]).
naoFrente(Pessoa4, Pessoa8, [[_,_,_], [Pessoa4,_], [_,_,Pessoa8]]).
naoFrente(Pessoa5, Pessoa1, [[Pessoa1,_,_], [_,Pessoa5], [_,_,_]]).
naoFrente(Pessoa5, Pessoa2, [[_,Pessoa2,_], [_,Pessoa5], [_,_,_]]).
naoFrente(Pessoa5, Pessoa3, [[_,_,Pessoa3], [_,Pessoa5], [_,_,_]]).
naoFrente(Pessoa5, Pessoa4, [[_,_,_], [Pessoa4,Pessoa5], [_,_,_]]).
naoFrente(Pessoa5, Pessoa6, [[_,_,_], [_,Pessoa5], [Pessoa6,_,_]]).
naoFrente(Pessoa5, Pessoa7, [[_,_,_], [_,Pessoa5], [_,Pessoa7,_]]).
naoFrente(Pessoa5, Pessoa8, [[_,_,_], [_,Pessoa5], [_,_,Pessoa8]]).
naoFrente(Pessoa6, Pessoa2, [[_,Pessoa2,_], [_,_], [Pessoa6,_,_]]).
naoFrente(Pessoa6, Pessoa3, [[_,_,Pessoa3], [_,_], [Pessoa6,_,_]]).
naoFrente(Pessoa6, Pessoa4, [[_,_,_], [Pessoa4,_], [Pessoa6,_,_]]).
naoFrente(Pessoa6, Pessoa5, [[_,_,_], [_,Pessoa5], [Pessoa6,_,_]]).
naoFrente(Pessoa6, Pessoa7, [[_,_,_], [_,_], [Pessoa6,Pessoa7,_]]).
naoFrente(Pessoa6, Pessoa8, [[_,_,_], [_,_], [Pessoa6,_,Pessoa8]]).
naoFrente(Pessoa7, Pessoa1, [[Pessoa1,_,_], [_,_], [_,Pessoa7,_]]).
naoFrente(Pessoa7, Pessoa3, [[_,_,Pessoa3], [_,_], [_,Pessoa7,_]]).
naoFrente(Pessoa7, Pessoa4, [[_,_,_], [Pessoa4,_], [_,Pessoa7,_]]).
naoFrente(Pessoa7, Pessoa5, [[_,_,_], [_,Pessoa5], [_,Pessoa7,_]]).
naoFrente(Pessoa7, Pessoa6, [[_,_,_], [_,_], [Pessoa6,Pessoa7,_]]).
naoFrente(Pessoa7, Pessoa8, [[_,_,_], [_,_], [_,Pessoa7,Pessoa8]]).
naoFrente(Pessoa8, Pessoa1, [[Pessoa1,_,_], [_,_], [_,_,Pessoa8]]).
naoFrente(Pessoa8, Pessoa2, [[_,Pessoa2,_], [_,_], [_,_,Pessoa8]]).
naoFrente(Pessoa8, Pessoa4, [[_,_,_], [Pessoa4,_], [_,_,Pessoa8]]).
naoFrente(Pessoa8, Pessoa5, [[_,_,_], [_,Pessoa5], [_,_,Pessoa8]]).
naoFrente(Pessoa8, Pessoa6, [[_,_,_], [_,_], [Pessoa6,_,Pessoa8]]).
naoFrente(Pessoa8, Pessoa7, [[_,_,_], [_,_], [_,Pessoa7,Pessoa8]]).

/*
verRestricoes(ListaRestricoes, OcupacaoMesa), atraves da recursao, vai verificar se todas
as restricoes da ListaRestricoes sao satisfeitas pela disposicao das pessoas nos lugares,
representada por OcupacaoMesa.
*/
verRestricoes([], _).
verRestricoes([Restricao|RestoDasRestricao], OcupacaoMesa) :-
    call(Restricao, OcupacaoMesa),
    verRestricoes(RestoDasRestricao, OcupacaoMesa).

/*
ocupacaoMesa(ListaPessoas, ListaRestricoes, OcupacaoMesa) vai gerar todas as possiveis permutacoes
de pessoas pelos lugares e, para cada permutacao, vai verificar se ela satisfaz todas as restricoes
fornecidas, atraves do predicado verRestricoes. Se encontrar uma permutacao valida, vai retornar 
essa permutacao que esta atribuida a variavel OcupacaoMesa.
*/

ocupacaoMesa(ListaPessoas, ListaRestricoes, OcupacaoMesa) :-
    permutation(ListaPessoas, [X1, X2, X3, X4, X5, X6, X7, X8]),
    OcupacaoMesa = [[X1, X2, X3], [X4, X5], [X6, X7, X8]],
    verRestricoes(ListaRestricoes, OcupacaoMesa).