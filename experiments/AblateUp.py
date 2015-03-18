#!/export/apps/bin/python
import os, time, math
from tools.tsub import Job
from tools.Misc import *

print
print "don't forget to compile!"
print

def setExperiment(job, type):
	if type == 'frank':
		job.setJavaOption('experiments', 'EECBTrainFrankTestExperiment')
	elif type == 'eecb':
		job.setJavaOption('experiments', 'OnlyEECBExperiment')
		job.setJavaOption('experiments.cv.folds', 5)
	else: raise Exception('invalid: ' + type)

sleep_between_subs = 0.5	# seconds
home = '/home/hltcoe/twolfe/miniScale2013/parma'
main_class = 'edu.jhu.parma.experiments.Pipeline'
lib_jars = all_jars_in(os.path.join(home, 'lib'))
lib_jars.append(os.path.join(home, 'target/scala-2.10/parma_2.10-1.0-SNAPSHOT.jar'))
lib_jars.append('/home/hltcoe/twolfe/scala/scala-2.10.0/lib/scala-library.jar')
cp = ':'.join(lib_jars) + ':'+home	# stuff like data/ and cache/ is under home
args = [os.path.join(home, 'parma.config')]

features = []
features.append('LemmaMatch')
features.append('ReportingVerbs')
features.append('SentenceContext')
features.append('wordnet.WordNetFeatures')
features.append('TEDAlignment')
features.append('DependencyParse')
features.append('AgigaHeadFeatures')
features.append('ppdb.PPDBFeatures')
features.append('JaroWinklerSimilarityFeature')
features.append('PositionalFeatures')
features.append('NumericSimilarity')
features.append('NameEntityStringMatch')
features.append('framenet.FrameNetFeatures')
features.append('wikirules.WikiRulesFeatures')
features.append('verb_pair.VerbEntailmentFeatures')
features.append(None)

jobs = []
for exp in ['eecb']:
#for exp in ['eecb', 'frank']:
	experimentDir = os.path.join(home, 'experiments', 'ablate_up', exp)
	for l1p in [1]:
	#for l1p in [10, 1, 0.1]:
		#for split in [False]:
		for split in [False, True]:
			for i, feat in enumerate(features):

				if feat is None:
					if split: continue
					if l1p != 1: continue

				name = ''

				name += 'StringMatch'
				all_features = 'StringMatch'
				if feat is not None:
					name += '_and-'+feat
					all_features += ' '+feat
				name += '_split-'+str(split)
				name += '_l1-'+str(l1p)

				j = Job(experimentDir, name, interactive=True)
				j.setJavaOption('features', all_features)
				j.setJavaOption('inference.ham.L1penalty', str(l1p))

				setExperiment(j, exp)

				if split:
					if feat is None: continue
					j.setJavaOption('features.split.predarg', feat)

				diag_dir = 'diagnostics'
				j.setJavaOption('diagnostics.parameter.outdir', j.getResourceDirectory(diag_dir, 'parameters'))
				j.setJavaOption('diagnostics.predictions.all', j.getResourceFile(diag_dir, 'predictions.all.txt'))
				j.setJavaOption('diagnostics.predictions.bad', j.getResourceFile(diag_dir, 'predictions.bad.txt'))
				j.setJavaOption('diagnostics.profile.file', j.getResourceFile(diag_dir, 'profile_times.txt'))
				j.setJavaOption('diagnostics.canonical.mention.file', j.getResourceFile(diag_dir, 'canonical_mentions.txt'))
				#j.setJavaOption('diagnostics.features.outdir', j.getResourceDirectory(diag_dir, 'alignment_features'))

				xmx = 5		# in GB
				mem_free = int(math.ceil(xmx * 3.6))
				j.setSubmission(cp, main_class, args, xmx=str(xmx)+'G', mem_free=str(mem_free)+'G', profile=False, asserts=True)
				print j.qsubScript()
				jobs.append(j)

# take random sample for testing
#import random
#random.shuffle(jobs)
#jobs = jobs[:30]

r = raw_input("there are %d jobs, would you like to submit? [y|n] " % (len(jobs)))
if r.lower() == 'y':
	for i, j in enumerate(jobs):
		if i > 0: time.sleep(sleep_between_subs)
		j.submit()
	os.system('qinfo')
else:
	print 'not submitting any jobs'





