mex agc.c -O
agc(ones(1,20),[.5;.5])
agc(ones(1,20),[.5;.5])
[output s] = agc(ones(1,10),[.5;.5])
agc(ones(1,10),[.5; .5], s)



mex agc.c -DINVERSE -output inverseagc -O
output = agc(ones(1,20),[.5;.5]);
inverseagc(output, [.5; .5])

mex soscascade.c -O
soscascade([1 0 0 0 0],[1 0 0 -.9 0;1 1 0 0 0]) 
soscascade([1 0 0 0 0 0 0 0 0 0],[1 0 0 -.9 0;1 1 0 0 0])
[y,s] = soscascade([1 0 0 0 0],[1 0 0 -.9 0;1 1 0 0 0])
soscascade([0 0 0 0 0],[1 0 0 -.9 0;1 1 0 0 0], s)

mex invsoscascade.c -O
invsoscascade([1 0 0 0 0;1 0 0 0 0],[1 0 0 -.9 0;1 1 0 0 0]) 

mex sosfilters.c -O
sosfilters([1 0 0 0 0 0], [1 0 0 -.9 0; 1 0 0 -.8 0])
sosfilters([1 0 0 0 0 0;2 0 0 0 0 0], [1 0 0 -.9 0])
sosfilters([1 zeros(1,9)], [1 0 0 -.9 0; 1 0 0 -.8 0])
[output,s] = sosfilters([1 zeros(1,4)], [1 0 0 -.9 0; 1 0 0 -.8 0])
sosfilters(zeros(1,5), [1 0 0 -.9 0; 1 0 0 -.8 0], s)
