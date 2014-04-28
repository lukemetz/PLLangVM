import sys, os

def cleanBuild(output):
	print "Cleaning previous build..."
	import shutil
	if not os.path.exists("build"):
            os.mkdir("build")
	if os.path.exists("build"):
		shutil.rmtree("build")

def compileLinux(output):
	name = output.split(".")[0]
	os.system("rlwrap sml run.sml " + output)
	print "Compiling with llvm..."
	os.system("llc " + "build/" + name + ".ll -o build/" + name + ".s")
	os.system("gcc " + "build/" + name + ".s" + " -o " + name)
	print "Compile Success"

def compileWin(output):
	print "Reading IR to LLVM"
	os.system("sml run.sml %s" % sys.argv[1])
	print "Compiling with llvm..."
	os.system("llc build\\{0}.ll -o build\\{0}.s &&\"C:\mingw\\bin\\gcc.exe\" build\{0}.s -o {0}".format(output[:output.rfind(".plg")]))
	print "Compile Success"

def compileOSX(output):
	print "Unimplemented in Mac OS X"
	pass

if __name__ == "__main__":
	filename = sys.argv[1]
	cleanBuild(filename)
	print os.path.dirname(filename)
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
