"""An API for MAPE components to communicate with the knowledge base"""
import hug

@hug.put('/monitor', versions=1)
def monitor(resource, status):
    """Indicate the status of a resource. The resource is identified by its resource ID."""
    return "OK"
