# DataLogTemp
Automated temporal repair project within Datalog  


Examples are in test_Souffle\basic 

TODO:
0. present how to use function tool. 
1. working examples 1.8, 3.15, 4.1, 5.0
-- 3.15_EGAF_togglecounter_true-valid-ltl.dl is NOT a bug, 
   because at the entry point 4, i's value are non-deterministic, 
   therefor it does not satisfy AF, therefore, it does not satisfy EGAF. 
   ??? WHAT IS THE DIFF BETWEEN EXITS A PATH AND NON-DETERMINISTIC ??? 
2. discuss about 3.14 example, -ctl "NOT{AG{{timer_1 = 0 -> AF{output_1 == 1}}}}" 
3. think about 2.4 and 3.9 examples 
4. (Martin) start on the transition rules. 
4.1 (Martin) explain the changes in the ctl2datalog file. 
5. (Liu Yu) start from the repair implementation. 

6. start on writing the fact generation rules and look for loop invariant generation tools. 