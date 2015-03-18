#!/export/apps/bin/python
import os, time
from tools.Misc import *
from tools.tsub import Job

print
print "don't forget to compile!"
print

sleep_between_subs = 1.5	# seconds
home = '/home/hltcoe/twolfe/miniScale2013/parma'
experimentDir = os.path.join(home, 'experiments/l1_sweep')
main_class = 'edu.jhu.parma.experiments.Pipeline'
lib_jars = all_jars_in(os.path.join(home, 'lib'))
lib_jars.append(os.path.join(home, 'target/scala-2.10/parma_2.10-1.0-SNAPSHOT.jar'))
lib_jars.append('/home/hltcoe/twolfe/scala/scala-2.10.0/lib/scala-library.jar')
cp = ':'.join(lib_jars)
args = [os.path.join(home, 'parma.config')]
jobs = []
for i, l1_val in enumerate([0.01, 0.1, 1.0, 10.0, 100.0, 1000.0]):
	name = 'l1_pow_' + str(i-2)
	j = Job(experimentDir, name, interactive=True)
	j.setJavaOption('inference.ham.L1penalty', l1_val)

	j.setJavaOption('diagnostics.parameter.outfile', j.getResourceFile('diagnostics', 'parameters.txt'))
	j.setJavaOption('diagnostics.predictions.all', j.getResourceFile('diagnostics', 'predictions.all.txt'))
	j.setJavaOption('diagnostics.predictions.bad', j.getResourceFile('diagnostics', 'predictions.bad.txt'))
	j.setJavaOption('diagnostics.profile.file', j.getResourceFile('diagnostics', 'profile_times.txt'))
	j.setJavaOption('diagnostics.canonical.mention.file', j.getResourceFile('diagnostics', 'canonical_mentions.txt'))

	j.setSubmission(cp, main_class, args, xmx='5G', mem_free='7G', profile=False, asserts=True)
	print j.qsubScript()
	jobs.append(j)

r = raw_input("there are %d jobs, would you like to submit? [y|n] " % (len(jobs)))
if r.lower() == 'y':
	for i, j in enumerate(jobs):
		if i > 0: time.sleep(sleep_between_subs)
		j.submit()
	os.system('qinfo')
else:
	print 'not submitting any jobs'





