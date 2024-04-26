# Cab Project

## Personal note
This is the cab project that I ppresented for my TIPE in 2021.

## Goal
The main goal of this project is to reduce the length of the route taken by a cab that needs to bring client to their destination in turns.

## Algorithms used

The project use the dijkstra algorithm or A* in order to find the shortest path between two points.
In order to find the shortest path, we need to consider the order in wich the clients will be taken care of.
There is an implementation of the nearest neighbor and an algorithm that use Darwin Theory in order to select the best path.
We see an average improvement of up to 10% for 20 clients.
