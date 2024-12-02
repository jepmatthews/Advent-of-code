def report_is_safe(report):
    diffs = list(map(lambda a,b: int(b) - int(a), report[0:-1],report[1:]))
    return all(map(lambda d: 0 < d <= 3, diffs)) or all(map(lambda d: 0 < -d <= 3, diffs))

f = open("../input.txt")
print(list((map(lambda lin: report_is_safe(lin.split()),f.readlines()))).count(True))