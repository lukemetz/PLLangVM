import sys, os

def cleanBuild(output):
	print "Cleaning previous build..."
	if os.path.exists("build"):
		for buildFile in os.listdir("build"):
			os.remove(os.path.join("build",buildFile))
	if os.path.exists(output):
		os.remove(output)

def compileLinux(output):
	os.system("rlwrap sml run.sml " + sys.argv[1])
	print "Compiling with llvm..."
	os.system("llc " + "build/" + output + ".ll -o build/" + output + ".s")
	os.system("gcc " + "build/" + output + ".s" + " -o " + output)
	print "Compile Success"

def compileWin(output):
	print "Reading IR to LLVM"
	os.system("sml run.sml %s" % sys.argv[1])
	print "Compiling with llvm..."
	os.system("llc build\\{0}.ll -o build\\{0}.s &&\"C:\mingw\\bin\\gcc.exe\" build\{0}.s -o {0}".format(output))
	print "Compile Success"

def compileOSX(output):
	print "Unimplemented in Mac OS X"
	pass

if __name__ == "__main__":
	filename = sys.argv[1].split(".")[0]
	cleanBuild(filename)
	if "win" in sys.platform:
		compileWin(filename)
	elif "darwin" in sys.platform:
		compileOSX(filename)
	elif "linux" in sys.platform:
		compileLinux(filename)
	else:
		print "What are you"
		pass
