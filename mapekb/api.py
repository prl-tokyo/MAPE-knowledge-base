import hug

@hug.put('/monitor', versions=1)
def monitor(status):
    return "OK"
