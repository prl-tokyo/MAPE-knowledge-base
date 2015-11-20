"""An API for MAPE components to communicate with the knowledge base"""
import hug

@hug.default_input_format("application/json")
def my_input_formatter(data):
    return(hug.input_format.json(data))

@hug.put('/monitor', versions=1)
def monitor(resource, status):
    """Indicate the status of a resource. The resource is identified by its resource ID."""
    return "OK"
