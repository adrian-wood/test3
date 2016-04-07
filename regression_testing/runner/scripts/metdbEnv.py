import re,os

def setEnv(file):
  envars={}
  try:
    FH = open(file,'r')
    lines = FH.readlines()
    for line in lines:
      if line.find('#') == -1 :
        parts = line.split('=')
        if len(parts) == 2:
          varName = parts[0]
          varVal = parts[1].strip()
          os.environ[varName] = varVal
          envars[varName] = varVal
  except:
# Handle errors but do nothing
    pass
  finally:
    if FH: FH.close()
  return envars

def subEnv(line):
# replace unix variables with values

  newLine=line
  it =  re.finditer(r"\$\w*", line)
  for match in it:
    envVar=match.group()[1:].strip()
    value=os.environ.get(envVar,"unknown")
    newLine=newLine.replace('$'+envVar,value)
  return newLine

if __name__ == '__main__':

  line = " this is $HOME/temp.file $USER spaces $METDB_BASE_DIR/filename.sh"
  print line
  print subEnv(line)
