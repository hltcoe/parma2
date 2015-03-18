#!/usr/bin/env python

import os, sys, math, subprocess, re
from tools.Misc import grep
from tools.tsub import Job
from tools.FeatureSelection import FeatureSelectionWithJobManagement

class ParmaForwardFeatureSelection:

	def __init__(self, features, cliArgs, home, pipelineClass, classpath):
		'''
		features is a list of strings, names of features
		home is where these experiments will live
		'''
		if not os.path.exists(home) or not os.path.isdir(home):
			print 'home doesnt exist:', home
			assert False
		self.features = features
		self.home = home
		self.pipelineClass = pipelineClass
		self.classpath = classpath
		self.cliArgs = cliArgs
		self.shortnames = {}
	
	def run(self, maxIter=8, minImprovement=0.02):
		ffs = FeatureSelectionWithJobManagement(self.evalJob, self.jobWithFeatures)
		bestFeatures, improvements = ffs.forwardSelection(self.features, \
			initFeatures=['LemmaMatch'], minImprovement=minImprovement, maxIter=maxIter)
		print 'best features =', bestFeatures
		print 'improvements =', improvements

	def evalJob(self, job):
		assert job.isFinished()
		if job.failed():
			print "WARNING: job %d failed (%s)" % (job.jid, job.home)
			return 0.0
		l = job.newestLog()
		print 'in RF evalJob function! logfile =', l
		assert l and os.path.exists(l) and os.path.isfile(l)
		# "averaged loss for EECB_CV_Experiment on 5 folds is 0.371, ci(z=2.0) = (0.363, 0.380)"
		# "averaged score EECB_CV_Experiment on 5 folds, argument-F1=0.704 ci(z=2.0) = (0.694, 0.713)"
		# "experiment=EECBTrainRFTestExperiment, corpus=trainEECB_testRFdev_wFeatures, loss=0.434"
		#regex = 'averaged loss for .+ is (\d+)\.(\d+).+'
		regex = 'experiment=\S+, corpus=\S+, loss=(\d+).(\d+)'
		line = grep(regex, l, pcre=True)[-1]
		m = re.match(regex, line)
		if m:
			return 1.0 - float('.'.join(m.groups()))
		else:
			raise Exception('i cant process: '+line)

	def jobFailed(self, logfile):
		print 'in RF jobFailed function, logfile =', logfile
		assert os.path.exists(logfile)
		assert os.path.isfile(logfile)
		r = subprocess.check_output("grep -m 1 -c Exception %s; exit 0" % (logfile), shell=True)
		return r.strip() != '0'

	def shortNameForFeature(self, f):
		if f in self.shortnames:
			return self.shortnames[f]
		for i in range(3, len(f)):
			s = f[:i]
			if s not in self.shortnames.values():
				self.shortnames[f] = s
				return s
		print 'self.shortnames =', self.shortnames
		print 'f =', f
		raise Exception()

	def jobWithFeatures(self, features):
		sn = [self.shortNameForFeature(x) for x in features]
		name = "ffs-%d-%s" % (len(features), '.'.join(sn))
		#print '[jobWithFeatures] features =', features
		#print '[jobWithFeatures] self.home =', self.home
		#print '[jobWithFeatures] name =', name
		j = Job(self.home, name, self.jobFailed, interactive=True)
		j.setJavaOption('features', ' '.join([str(x) for x in features]))
		j.setJavaOption('experiments', 'EECBTrainFrankTestExperiment')
		xmx = math.ceil(1.5 + 0.5*len(features))
		mem_free = max(xmx + 1.0, xmx * 1.5)
		j.setSubmission(self.classpath, self.pipelineClass, self.cliArgs, xmx=xmx, mem_free=mem_free)
		return j

home = '/home/hltcoe/twolfe/'
projectRoot = os.path.join(home, 'miniScale2013/parma/')
experimentRoot = os.path.join(projectRoot, 'experiments/output/rf-comparison/')
parmaConfig = os.path.join(projectRoot, 'parma.basic.config')
classpath = subprocess.check_output(os.path.join(projectRoot, 'classpath.sh')).strip()
pipelineClass = 'edu.jhu.parma.experiments.PipelineRunner'

def runLemma(dataset):
	assert dataset in ['dev', 'test']
	name = 'lemma-'+dataset
	print "[runLemma] name=" + name
	print "[runLemma] experimentRoot=" + experimentRoot
	j = Job(experimentRoot, name, jobFailed, interactive=True)
	j.setJavaOption('experiments', 'RFLemma'+dataset.title()+'Set')	# TODO make sure this exists
	j.setSubmission(classpath, pipelineClass, [parmaConfig], xmx='1G', mem_free='2G')
	j.submit()
	return j

def runForwardSelection(maxIter=8):
	# LemmaMatch is automatically included
	features = []
	features.append('JaroWinklerSimilarityFeature')
	features.append('DependencyParse')
	features.append('TransducerSimilarityFeature')
	features.append('ReportingVerbs')
	features.append('SentenceContext')
	features.append('TEDAlignment')
	features.append('wordnet.WordNetFeatures')
	features.append('ppdb.PPDBFeatures')
	features.append('framenet.FrameNetFeatures')
	#features.append('wikirules.WikiRulesFeatures')	# this is slow as crap to load up
	features.append('AgigaHeadFeatures')
	features.append('PositionalFeatures')
	features.append('NameEntityStringMatch')
	features.append('NumericSimilarity')
	features.append('verb_pair.VerbEntailmentFeatures')
	pffs = ParmaForwardFeatureSelection(features, [parmaConfig], experimentRoot, pipelineClass, classpath)
	pffs.run(maxIter)

if __name__ == '__main__':
	
	print 'compiling....'
	subprocess.check_call(os.path.join(projectRoot, 'make_jar.sh'))

	#runLemma('dev')
	runForwardSelection(2)





