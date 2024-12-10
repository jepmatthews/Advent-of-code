f = open("../input.txt")

line = f.readline()

memory = []

freeSpace = False
fileIndex = 0
for char in line:
    if(freeSpace):
        memory += (["."] * int(char))
        freeSpace = False
    else:
        memory += ([str(fileIndex)] * int(char))
        fileIndex += 1
        freeSpace = True

defragmentCursor = len(memory) - 1

while(defragmentCursor >= 0):
    while(defragmentCursor >= 0 and memory[defragmentCursor] == "." ):
        defragmentCursor -= 1    
    currentFileId = memory[defragmentCursor]
    fileLength = 1
    while(defragmentCursor >= 1 and memory[defragmentCursor-1] == currentFileId):
        defragmentCursor -= 1
        fileLength += 1
    startFileIndex = defragmentCursor
    freeMemoryCursor = 0
    freeMemoryLength = 0
    while(freeMemoryCursor < startFileIndex and freeMemoryLength < fileLength):
        while(freeMemoryCursor < startFileIndex and memory[freeMemoryCursor] != "."):
            freeMemoryCursor += 1
        startFreeIndex = freeMemoryCursor
        freeMemoryLength = 0
        while(freeMemoryCursor < startFileIndex and memory[freeMemoryCursor] == "."):
            freeMemoryLength += 1
            freeMemoryCursor += 1
    if freeMemoryLength >= fileLength:
        memory[startFreeIndex:startFreeIndex + fileLength] = memory[startFileIndex: startFileIndex + fileLength]
        memory[startFileIndex: startFileIndex + fileLength] = ["."]*fileLength
    if(defragmentCursor >= 0):
        defragmentCursor -= 1

result = 0

for index,val in enumerate(memory):
    if(val == "."):
        continue
    result += index * int(val)

print (result)

