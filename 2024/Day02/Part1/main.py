def report_is_safe(line):
    prevDiff = 0
    report = line.split()
    for (index, n) in list(enumerate(report))[1:]:
        diff = int(n) - int(report[index-1])
        if(diff * prevDiff < 0 or abs(diff) > 3 or diff == 0):
            return False
        prevDiff = diff
    return True

f = open("../input.txt")
print(list((map(report_is_safe,f.readlines()))).count(True))