numTrials = 10

import os
for i in xrange(numTrials):
	f = open('sentinelFile', 'a')
	f.write('%s\n' % (i))
	f.close()
	os.system('cat software-comparison.m2 | M2')
	os.remove('sentinelFile')
