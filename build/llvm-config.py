import subprocess
import sys

def main(args):
  output = subprocess.check_output(args)
  for opt in output.split():
    print opt

if __name__ == '__main__':
  sys.exit(main(sys.argv[1:]))
