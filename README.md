# DOSP Project 2: Gossip Simulation
****
## Group Details
<p><strong>Name: [ Pradyumna_Pasumarty : Sai Ram Varma_Budharaju ]</strong></p>
<strong>Members</strong>
<ol>
    <li>Sai Ram Varma Budharaju (UFID: 3337-4276 | <a>sbudharaju@ufl.edu</a>)</li>
    <li>Pradyumna Pasumarty (UFID: 6907-9707 | <a>ppasumarty@ufl.edu</a>)</li>
</ol>

****

## Project Description

The main goal of this project is to implement Gossip type algorithms used for aggregate computation and group communication. This project  determines the convergence of gossip and push-sum algorithm over various network topologies through a simulator in erlang.

****

## Design Details

<ul>
    <li>
        Gossip Algorithm
        <ul>
            <li>In this algorithm the starting node is picked at random from the available nodes.</li>
            <li>A list of other nodes (neighbors) is maintained for each node, to track the convergence</li>
            <li>The starting node randomly selects another node from its neighbors and passes the rumor to it</li>
            <li>Once the other node receives information from the first node it accepts the message and sends it to another node in a similar fashion as above.</li>
             <li>Each actor keeps the count of the number of times it has heard the rumor.</li>
             <li>In this project we set the threshold as 10 so when each node listens to the rumor 10 times(this value can be anything) it stops transmitting and convergence of entire network is achieved.</li>
        </ul>
    </li>
    <br/>
    <li>
        Push-Sum Algorithm
        <ul>
            <li>This algorithm is based on Gossip algorithm</li>
            <li>This algorithm works on interactive pairwise distribution of aggregated values among entities</li>
            <li>Each node's state stores two values, averaged sum and weight. The sum of a node is set to its index. And the weight is set to 1 at the beginning of convergence.</li>
            <li>These values are updated each iteration in terms of information from nodes.</li>
            <li>Each node sends half the value of its sum and weight while retaining half of the values in its own state.</li>
            <li>Each process calculates the average estimation by the difference of new-sum/new-weight ratio from its existing ratio.</li>
        </ul>
    </li>
</ul>
<br/>

****
##  What is working ?
<br/>
This project successfully implemented line,full,2D and imperfect 3D topologies for both gossip and push-sum algorithm.
<br/>
<br/>
Gossip: With gossip algorithm protocol, Convergence is achieved when each node listens to the message for 10 times. After convergence, the nodes stop message passing and the system prints the wall clock time it took for convergence.
<br/>
<br/>
Push-Sum: The Push-Sum network works by sending message s and w as parameter to an actor. The intial value of s is equal to the index of the actor and w = 1. The propagation stops when an actor's s/w ratio does not change for 3 times in a row (i.e stays within limit of 10^-10)
<br/>
<ul>
    <b>Observations</b>
    <li>Full topology is the fastest to converge and spread the rumor to all its notes.Full topology is connected to all its nodes and convergence is achieved in a very short time.</li>
    <li>Line topology takes the highest amount of time as it has access to only two neighbors ie., left and right node.</li>
    <li>Imperfect 3D will achieve convergence relatively faster that line and 2D.</li>
    <li>2D grip converges in between line and imperfect 3D grid as it has access to only its adjacent nodes</li>
</ul>
<i>Note: Push Sum can give 100% convergence as we try to reduce the value of s/w ratio till it stop changing for three times consecutively.</i>

## What is the largest network you managed to deal with for each type of topology and algorithm

<ol>
    <li>The largest network that we tested the convergence for both gossip and push-sum protocols in Full,imperfect 3D grid topologies is for 10000 nodes.</li>
    <li>The largest network that we tested the convergence for both gossip and push-sum protocols in line is for 2000 nodes.</li>
    <li>The project can work for higher number of nodes for other topologies except for line topology as line takes too much time to converge.</li>
    <li>A significant performance improvement is observed when the Actor Model is implemented as synchronous gossip takes a highly impractical amount of time to converge</li>
</ol>

### Largest networks and Real Times
Gossip:
<br/>
<table>
    <th>Topology</th>
    <th>Number of nodes</th>
    <th>Real Time (milliseconds)</th>
    <tr>
        <td>Full</td>
        <td>10000</td>
        <td>154593</td>
    </tr>
    <tr>
        <td>Imperfect 3D grid</td>
        <td>10000</td>
        <td>160215</td>
    </tr>
    <tr>
        <td>2D grid</td>
        <td>10000</td>
        <td>7033959</td>
    </tr>
    <tr>
        <td>Line</td>
        <td>2000</td>
        <td>36652</td>
    </tr>
</table>
Push-Sum:
<br/>
<br/>
<table>
    <th>Topology</th>
    <th>Number of nodes</th>
    <th>Real Time (milliseconds)</th>
    <tr>
        <td>Full</td>
        <td>10000</td>
        <td>235263</td>
    </tr>
    <tr>
        <td>Imperfect 3D grid</td>
        <td>10000</td>
        <td>306199</td>
    </tr>
    <tr>
        <td>2D grid</td>
        <td>10000</td>
        <td>9160163</td>
    </tr>
    <tr>
        <td>Line</td>
        <td>2000</td>
        <td>3352343</td>
    </tr>
</table>
<img src="Gossip Normal Scale.PNG"/>>
<br/>
<img src="Gossip Log Scale.PNG"/>
<br/>
<br/>
<img src="Push Sum Normal Scale.PNG"/>
<br/>
<img src="Push Sum Log Scale.PNG"/>

****

## Observations
<ol>
<li>
For both the protocols full topology converges very fast as it has access to all nodes in the network. As the rumor passes the probability of passing rumor succesfully increases exponentially.
</li>
<li>
Line algorithm is the topology that takes highest amount of time because of the limited access to its neighbours
</li>
</ol>

****

## Execution Screenshots for 1000 Nodes

<strong>Gossip: </strong>
<br/>
<br/>
<img src="Gossip Line.PNG"/>
<img src="Gossip TwoD.PNG"/>
<img src="Gossip 3D.PNG"/>
<img src="Gossip Full.PNG"/>
<br/>
<br/>
<strong>Push Sum: </strong>
<br/>
<br/>
<img src="Push Sum Line.PNG"/>
<img src="Push Sum TwoD.PNG"/>
<img src="Push Sum 3D.PNG"/>
<img src="Push Sum Full.PNG"/>


