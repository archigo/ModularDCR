A -->* B      
2			Amount of events
0 1 0 A		Executed Included Pending Name
0 1 0 B
0 1 3		Relations: from to Type: 0=include(-->+) 1=Exclude(-->%) 2=Response(*-->) 3=Condition(-->*) 4=Milestone(--<>)

!A -->% B
2
0 1 1 A
0 1 0 B
0 1 1

!A -->% B -->* C
3
0 1 1 A
0 1 0 B
0 1 0 C
0 1 1
1 2 3

!A -->% B -->* %C
3
0 1 1 A
0 1 0 B
0 0 0 C
0 1 1
1 2 3

Q -->+ !A -->% B -->* %C
4
0 1 1 A
0 1 0 B
0 0 0 C
0 1 0 Q
3 0 0
0 1 1
1 2 3

