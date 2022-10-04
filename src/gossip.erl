-module(gossip).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/1]).
-export([
  runForeverRecursively/0, startGossipSimulation/3, setTopology/2,
  setGossipOrPushSum/2, initializeGossip/1, performGossipRecursively/2,
  createFullTopology/1, createLineTopology/1
]).
-export([getIndex/1, getRumourCount/1, getNeighbors/1, getXCoordinate/1, getYCoordinate/1]).
-export([updateIndex/2, updateRumourCount/2, updateNeighbors/2, updateXCoordinate/2, updateYCoordinate/2]).

-define(SERVER, ?MODULE).

runForeverRecursively() ->
  runForeverRecursively().

start_link() ->
  {ok, Pid} = gen_server:start_link(gossip, [], []),
  Pid.

init([]) ->
  {ok, {0,0,[],0,0,1}}.

getIndex(Pid) ->
  gen_server:call(Pid, {getIndex, []}).

getRumourCount(Pid) ->
  gen_server:call(Pid, {getRumourCount, []}).

getNeighbors(Pid) ->
  gen_server:call(Pid, {getNeighbors, []}).

getXCoordinate(Pid) ->
  gen_server:call(Pid, {getXCoordinate, []}).

getYCoordinate(Pid) ->
  gen_server:call(Pid, {getYCoordinate, []}).

updateIndex(Pid, Index) ->
  gen_server:call(Pid, {updateIndex, Index}).

updateRumourCount(Pid, NodeList) ->
  gen_server:call(Pid, {updateRumourCount, Pid, NodeList}).

updateNeighbors(Pid, Neighbors) ->
  gen_server:call(Pid, {updateNeighbors, Neighbors}).

updateXCoordinate(Pid, XCoordinate) ->
  gen_server:call(Pid, {updateXCoordinate, XCoordinate}).

updateYCoordinate(Pid, YCoordinate) ->
  gen_server:call(Pid, {updateXCoordinate, YCoordinate}).

handle_call({getIndex, []}, _, State) ->
  {Index, _, _, _, _, _} = State,
  {reply, Index, State};

handle_call({getRumourCount, []}, _, State) ->
  {_, RumourCount, _, _, _, _} = State,
  {reply, RumourCount, State};

handle_call({getNeighbors, []}, _, State) ->
  {_, _, NeighborNodes, _, _, _} = State,
  {reply, NeighborNodes, State};

handle_call({getXCoordinate, []}, _, State) ->
  {_, _, _, XCoordinate, _, _} = State,
  {reply, XCoordinate, State};

handle_call({getYCoordinate, []}, _, State) ->
  {_, _, _, _, YCoordinate, _} = State,
  {reply, YCoordinate, State};

handle_call({updateIndex, NewIndex}, _, State) ->
  {Index, RumourCount, NeighborNodes, XCoordinate, YCoordinate, Weight} = State,
  NewState = {NewIndex, RumourCount, NeighborNodes, XCoordinate, YCoordinate, Weight},
  {reply, Index, NewState};

handle_call({updateRumourCount, Pid, NodeList}, _, State) ->
  {Index, RumourCount, NeighborNodes, XCoordinate, YCoordinate, Weight} = State,
  [{_, TotalCount}] = ets:lookup(my_table,finishedNodeCount),
  if
    TotalCount == length(NodeList) ->
      {_, RunTimeTakenSinceLastCall} = statistics(wall_clock),
      io:fwrite("Total count: ~p reached in ~p milliseconds ~n", [TotalCount, RunTimeTakenSinceLastCall]),
      exit(Pid, normal);
    true ->
      NewRumourCount = RumourCount + 1,
      NewState = {Index, NewRumourCount, NeighborNodes, XCoordinate, YCoordinate, Weight},
      {reply, NewRumourCount, NewState}
  end;

handle_call({updateNeighbors, NewNeighbors}, _, State) ->
  {Index, RumourCount, NeighborNodes, XCoordinate, YCoordinate, Weight} = State,
  UpdatedNeighborsList = NeighborNodes ++ [NewNeighbors],
  NewState = {Index, RumourCount, UpdatedNeighborsList, XCoordinate, YCoordinate, Weight},
  {reply, Index, NewState};

handle_call({updateXCoordinate, NewXCoordinate}, _, State) ->
  {Index, RumourCount, NeighborNodes, XCoordinate, YCoordinate, Weight} = State,
  NewState = {Index, RumourCount, NeighborNodes, NewXCoordinate, YCoordinate, Weight},
  {reply, XCoordinate, NewState};

handle_call({updateYCoordinate, NewYCoordinate}, _, State) ->
  {Index, RumourCount, NeighborNodes, XCoordinate, YCoordinate, Weight} = State,
  NewState = {Index, RumourCount, NeighborNodes, XCoordinate, NewYCoordinate, Weight},
  {reply, YCoordinate, NewState};

handle_call(stopProcess, _From, State) ->
  {stop, normal, shutdown_ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(Pid) ->
  gen_server:stop(Pid).

startGossipSimulation(NumberOfNodes, RequiredTopology, GossipOrPushSum) ->
  NodeList = lists:foldl(
    fun(Index, Accumulator) ->
      Pid = start_link(),
      updateIndex(Pid, Index),
      Accumulator ++ [Pid]
    end,
    [],
    lists:seq(1, NumberOfNodes)
  ),
  setTopology(RequiredTopology, NodeList),
  setGossipOrPushSum(GossipOrPushSum, NodeList).

setTopology(RequiredTopology, NodeList) ->
  case RequiredTopology of
    line ->
      createLineTopology(NodeList);
    full ->
      createFullTopology(NodeList);
    twoD ->
      ok;
    imperfTwoD ->
      ok
  end.

setGossipOrPushSum(GossipOrPushSum, NodeList) ->
  case GossipOrPushSum of
    gossip ->
      initializeGossip(NodeList);
    pushSum ->
      ok
  end.

createLineTopology(NodeList) ->
  Function =
    fun(Pid) ->
      IndexOfPid = string:str(NodeList, [Pid]),
      lists:foreach(
        fun(NeighborToAdd) ->
          IndexOfInnerNode = string:str(NodeList, [NeighborToAdd]),
          if
            IndexOfInnerNode == IndexOfPid + 1 ->
              updateNeighbors(Pid, NeighborToAdd);
            IndexOfInnerNode == IndexOfPid - 1 ->
              updateNeighbors(Pid, NeighborToAdd);
            true ->
              ok
          end
        end,
        NodeList
      )
    end,
  statistics(wall_clock),
  lists:foreach(Function, NodeList),
  {_, RunTimeTakenSinceLastCall} = statistics(wall_clock),
  io:fwrite("Time taken to prepare topology ~p milliseconds ~n", [RunTimeTakenSinceLastCall]).


createFullTopology(NodeList) ->
  Function =
    fun(Pid) ->
      IndexOfPid = string:str(NodeList, [Pid]),
      lists:foreach(
        fun(NeighborToAdd) ->
          IndexOfInnerNode = string:str(NodeList, [NeighborToAdd]),
          if
            IndexOfInnerNode =/= IndexOfPid ->
              updateNeighbors(Pid, NeighborToAdd);
            true ->
              ok
          end
        end,
        NodeList
      )
    end,
  statistics(wall_clock),
  lists:foreach(Function, NodeList),
  {_, RunTimeTakenSinceLastCall} = statistics(wall_clock),
  io:fwrite("Time taken to prepare topology ~p milliseconds ~n", [RunTimeTakenSinceLastCall]).

initializeGossip(NodeList) ->
  statistics(wall_clock),
  RandomNodeToStart = lists:nth(rand:uniform(length(NodeList)), NodeList),
  ets:new(my_table, [named_table, public, set, {keypos, 1}]),
  ets:insert(my_table, {finishedNodeCount, 0}),
  updateRumourCount(RandomNodeToStart, NodeList),
  performGossipRecursively(RandomNodeToStart, NodeList).

performGossipRecursively(RandomNode, NodeList) ->
  CurrentRumourCount = getRumourCount(RandomNode),
  NeighborsOfNode = getNeighbors(RandomNode),
  if
    CurrentRumourCount == 10 ->
      ets:update_counter(my_table, finishedNodeCount, {2,1});
    true ->
      ok
  end,
  RandomNeighborNode = lists:nth(rand:uniform(length(NeighborsOfNode)), NeighborsOfNode),
  updateRumourCount(RandomNeighborNode, NodeList),
  performGossipRecursively(RandomNeighborNode, NodeList).