import sys
import os

output = sys.argv[1].split(".")[0]
if os.path.exists(output):
    os.remove(output)
if os.path.exists(output+".ll"):
    os.remove(output + ".ll")
if os.path.exists(output + ".s"):
    os.remove(output + ".s")

os.system("rlwrap sml run.sml " + sys.argv[1])
os.system("sml run.sml " + sys.argv[1])
print "Compiling with llvm..."
os.system("llc " + output + ".ll")
os.system("gcc " + output + ".s" + " -o " + output)
print "Compile Success"
