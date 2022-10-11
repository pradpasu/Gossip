-module(gossip).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/1]).
-export([
  runForeverRecursively/0, startGossipSimulation/3, setTopology/2,
  setGossipOrPushSum/2, initializeGossip/1, performGossipRecursively/2,
  createFullTopology/1, createLineTopology/1
]).
-export([getIndex/1, getRumourCount/1, getNeighbors/1, getXCoordinate/1, getYCoordinate/1, getSum/1, getWeight/1]).
-export([updateIndex/2, updateRumourCount/2, updateNeighbors/2, updateXCoordinate/2, updateYCoordinate/2, updateSum/2, updateWeight/2]).

-define(SERVER, ?MODULE).

runForeverRecursively() ->
  runForeverRecursively().

start_link() ->
  {ok, Pid} = gen_server:start_link(gossip, [], []),
  Pid.

init([]) ->
  {ok, {0,0,[],0,0,0,1}}.

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

getSum(Pid) ->
  gen_server:call(Pid, {getSum, []}).

getWeight(Pid) ->
  gen_server:call(Pid, {getWeight, []}).

updateIndex(Pid, Index) ->
  gen_server:call(Pid, {updateIndex, Index}).

updateRumourCount(Pid, NodeList) ->
  gen_server:call(Pid, {updateRumourCount, Pid, NodeList}).

updateNeighbors(Pid, Neighbors) ->
  gen_server:call(Pid, {updateNeighbors, Neighbors}).

updateXCoordinate(Pid, XCoordinate) ->
  gen_server:call(Pid, {updateXCoordinate, XCoordinate}).

updateYCoordinate(Pid, YCoordinate) ->
  gen_server:call(Pid, {updateYCoordinate, YCoordinate}).

updateSum(Pid, Sum) ->
  gen_server:call(Pid, {updateSum, Sum}).

updateWeight(Pid, Weight) ->
  gen_server:call(Pid, {updateWeight, Weight}).

handle_call({getIndex, []}, _, State) ->
  {Index, _, _, _, _, _, _} = State,
  {reply, Index, State};

handle_call({getRumourCount, []}, _, State) ->
  {_, RumourCount, _, _, _, _, _} = State,
  {reply, RumourCount, State};

handle_call({getNeighbors, []}, _, State) ->
  {_, _, NeighborNodes, _, _, _, _} = State,
  {reply, NeighborNodes, State};

handle_call({getXCoordinate, []}, _, State) ->
  {_, _, _, XCoordinate, _, _, _} = State,
  {reply, XCoordinate, State};

handle_call({getYCoordinate, []}, _, State) ->
  {_, _, _, _, YCoordinate, _, _} = State,
  {reply, YCoordinate, State};

handle_call({getSum, []}, _, State) ->
  {_, _, _, _, _, Sum, _} = State,
  {reply, Sum, State};

handle_call({getWeight, []}, _, State) ->
  {_, _, _, _, _, _, Weight} = State,
  {reply, Weight, State};

handle_call({updateIndex, NewIndex}, _, State) ->
  {Index, RumourCount, NeighborNodes, XCoordinate, YCoordinate, _, Weight} = State,
  NewState = {NewIndex, RumourCount, NeighborNodes, XCoordinate, YCoordinate, NewIndex, Weight},
  {reply, Index, NewState};

handle_call({updateRumourCount, Pid, NodeList}, _, State) ->
  {Index, RumourCount, NeighborNodes, XCoordinate, YCoordinate, Sum, Weight} = State,
  [{_, TotalCount}] = ets:lookup(my_table,finishedNodeCount),
  if
    TotalCount == length(NodeList) ->
      {_, RunTimeTakenSinceLastCall} = statistics(wall_clock),
      io:fwrite("Total count: ~p reached in ~p milliseconds ~n", [TotalCount, RunTimeTakenSinceLastCall]),
      exit(Pid, normal);
    true ->
      NewRumourCount = RumourCount + 1,
      NewState = {Index, NewRumourCount, NeighborNodes, XCoordinate, YCoordinate, Sum, Weight},
      {reply, NewRumourCount, NewState}
  end;

handle_call({updateNeighbors, NewNeighbors}, _, State) ->
  {Index, RumourCount, NeighborNodes, XCoordinate, YCoordinate, Sum, Weight} = State,
  UpdatedNeighborsList = NeighborNodes ++ [NewNeighbors],
  NewState = {Index, RumourCount, UpdatedNeighborsList, XCoordinate, YCoordinate, Sum, Weight},
  {reply, Index, NewState};

handle_call({updateXCoordinate, NewXCoordinate}, _, State) ->
  {Index, RumourCount, NeighborNodes, XCoordinate, YCoordinate, Sum, Weight} = State,
  NewState = {Index, RumourCount, NeighborNodes, NewXCoordinate, YCoordinate, Sum, Weight},
  {reply, XCoordinate, NewState};

handle_call({updateYCoordinate, NewYCoordinate}, _, State) ->
  {Index, RumourCount, NeighborNodes, XCoordinate, YCoordinate, Sum, Weight} = State,
  NewState = {Index, RumourCount, NeighborNodes, XCoordinate, NewYCoordinate, Sum, Weight},
  {reply, YCoordinate, NewState};

handle_call({updateSum, NewSum}, _, State) ->
  {Index, RumourCount, NeighborNodes, XCoordinate, YCoordinate, Sum, Weight} = State,
  NewState = {Index, RumourCount, NeighborNodes, XCoordinate, YCoordinate, NewSum, Weight},
  {reply, Sum, NewState};

handle_call({updateWeight, NewWeight}, _, State) ->
  {Index, RumourCount, NeighborNodes, XCoordinate, YCoordinate, Sum, Weight} = State,
  NewState = {Index, RumourCount, NeighborNodes, XCoordinate, YCoordinate, Sum, NewWeight},
  {reply, Weight, NewState};

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
  statistics(wall_clock),
  setTopology(RequiredTopology, NodeList),
  {_, RunTimeTakenSinceLastCall} = statistics(wall_clock),
  io:fwrite("Time taken to prepare topology ~p milliseconds ~n", [RunTimeTakenSinceLastCall]),
  setGossipOrPushSum(GossipOrPushSum, NodeList).

setTopology(RequiredTopology, NodeList) ->
  case RequiredTopology of
    line ->
      createLineTopology(NodeList);
    full ->
      createFullTopology(NodeList);
    twoD ->
      create2DTopology(NodeList);
    imperf3D ->
      create2DTopology(NodeList),
      addRandomNeighborsToGrid(NodeList)
  end.

setGossipOrPushSum(GossipOrPushSum, NodeList) ->
  case GossipOrPushSum of
    gossip ->
      initializeGossip(NodeList);
    pushSum ->
      initializePushSum(NodeList)
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
  lists:foreach(Function, NodeList).


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
  lists:foreach(Function, NodeList).

create2DTopology(NodeList) ->
  GridSettingFunction =
    fun(Pid) ->
      IndexOfPid = string:str(NodeList, [Pid]),
      Dividend = IndexOfPid div 4,
      Remainder = IndexOfPid rem 4,
      if
        Remainder =:= 0 ->
          updateXCoordinate(Pid, Dividend),
          updateYCoordinate(Pid, Remainder + 4);
        true ->
          updateXCoordinate(Pid, Dividend + 1),
          updateYCoordinate(Pid, Remainder)
      end
    end,
  NeighbourSettingFunction =
    fun(Pid) ->
      IndexOfPid = string:str(NodeList, [Pid]),
      lists:foreach(
        fun(NeighborToAdd) ->
          IndexOfInnerNode = string:str(NodeList, [NeighborToAdd]),
          if
            IndexOfInnerNode == IndexOfPid ->
              ok;
            true ->
              XofPid = getXCoordinate(Pid),
              YofPid = getYCoordinate(Pid),
              XofNeighbour = getXCoordinate(NeighborToAdd),
              YofNeighbour = getYCoordinate(NeighborToAdd),
              if
                (XofPid - 1 > 0) and (XofPid - 1 =:= XofNeighbour) and (YofPid =:= YofNeighbour)->
                  updateNeighbors(Pid, NeighborToAdd);
                (XofPid + 1 =< (length(NodeList) / 4)) and (XofPid + 1 =:= XofNeighbour) and (YofPid =:= YofNeighbour) ->
                  updateNeighbors(Pid, NeighborToAdd);
                (YofPid - 1 > 0) and (YofPid - 1 =:= YofNeighbour) and (XofPid =:= XofNeighbour)->
                  updateNeighbors(Pid, NeighborToAdd);
                (YofPid + 1 =< 4) and (YofPid + 1 =:= YofNeighbour) and (XofPid =:= XofNeighbour)->
                  updateNeighbors(Pid, NeighborToAdd);
                true ->
                  ok
              end
          end
        end,
        NodeList
      )
    end,
  lists:foreach(GridSettingFunction, NodeList),
  lists:foreach(NeighbourSettingFunction, NodeList).

addRandomNeighborsToGrid(NodeList) ->
  Function =
    fun(Pid) ->
      updateNeighbors(Pid, lists:nth(rand:uniform(length(NodeList)), NodeList))
    end,
  lists:foreach(Function, NodeList).

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

initializePushSum(NodeList) ->
  statistics(wall_clock),
  RandomNodeToStart = lists:nth(rand:uniform(length(NodeList)), NodeList),
  ets:new(my_table, [named_table, public, set, {keypos, 1}]),
  ets:insert(my_table, {finishedNodeCount, 0}),
  performPushSumRecursively(RandomNodeToStart, NodeList, 0, 0).

performPushSumRecursively(RandomNode, NodeList, SumToAdd, WeightToAdd) ->
  Sum = getSum(RandomNode),
  Weight = getWeight(RandomNode),
  RumourCount = getRumourCount(RandomNode),
  Neighbors = getNeighbors(RandomNode),
  NewSum = Sum + SumToAdd,
  NewWeight = Weight + WeightToAdd,
  Difference = abs((NewSum/NewWeight) - (Sum/Weight)),
  MaxDifference = math:pow(10,-10),
  SumToPass = NewSum / 2,
  WeightToPass = NewWeight / 2,
  if
    Difference =< MaxDifference ->
      updateRumourCount(RandomNode, NodeList),
      if
        RumourCount =:= 3 ->
          ets:update_counter(my_table, finishedNodeCount, {2,1}),
          [{_, TotalCount}] = ets:lookup(my_table,finishedNodeCount),
          if
            TotalCount == length(NodeList) ->
              {_, RunTimeTakenSinceLastCall} = statistics(wall_clock),
              io:fwrite("Total count: ~p reached in ~p milliseconds ~n", [TotalCount, RunTimeTakenSinceLastCall]),
              exit(RandomNode, normal);
            true ->
              ok
          end;
        true ->
          ok
      end;
    true ->
      ok
  end,
  updateSum(RandomNode, SumToPass),
  updateWeight(RandomNode, WeightToPass),
  RandomNeighbor = lists:nth(rand:uniform(length(Neighbors)), Neighbors),
  performPushSumRecursively(RandomNeighbor, NodeList, SumToPass, WeightToPass).