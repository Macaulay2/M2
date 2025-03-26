import os

numTrials = 10


for i in range(numTrials):
    f = open('sentinelFile', 'a')
    f.write('%s\n' % (i))
    f.close()
    os.system('cat software-comparison.m2 | M2')
    os.remove('sentinelFile')
