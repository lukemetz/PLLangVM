import unittest
from run import *
from subprocess import call
import sys

def run_sml(filename):
	# cleanBuild(filename)
	if not os.path.exists(os.path.join("build", os.path.dirname(filename))):
		os.makedirs(os.path.join("build", os.path.dirname(filename)))
	if "win" in sys.platform:
		compileWin(filename)
	elif "darwin" in sys.platform:
		compileOSX(filename)
	elif "linux" in sys.platform:
		compileLinux(filename)
	else:
		print "What are you"
		pass

	executable = ".".join(filename.split(".")[:-1])
	output_file = executable+"_output"
	call(executable, stdout=open(output_file,'wb+'))
	result = open(output_file, "r").read().strip()

	return result
	
class Test(unittest.TestCase):
	def test_basic(self):
		self.assertEquals(run_sml(os.path.join("tests","basic.plg")), "2")
	def test_if(self):
		self.assertEquals(run_sml("tests/if.plg"), "4")
	def test_curry(self):
		self.assertEquals(run_sml("tests/curry.plg"), "9")
	def test_fib(self):
		self.assertEquals(run_sml("tests/fib.plg"), "89")
	def test_anon(self):
		self.assertEquals(run_sml("tests/anon.plg"), "42")
if __name__ == "__main__":
	unittest.main()