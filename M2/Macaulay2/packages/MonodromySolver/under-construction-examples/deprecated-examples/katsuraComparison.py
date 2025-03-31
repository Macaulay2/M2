import os

numTrials = 500


for i in [5,6,7,8,9,10]:
    f = open('sentinelFile', 'a')
    f.write('%s\n' % (i))
    f.close()
    h = open('katsuraComparison.output', 'a')
    h.write('%s\n' % (i))
    h.close()
    for j in range(numTrials):
        g = open('sentinelFile2', 'a')
        g.write('%s\n' % (j))
        g.close()
        os.system('cat katsuraComparison.m2 | M2')
        os.remove('sentinelFile2')
    os.remove('sentinelFile')
