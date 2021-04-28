function isConnected(a, b) {
    if (writeBetween(a, b))
        return true

    for (x in outgoingConnections) {
        if (isConnected(x, b))
            return true
    }

    return false
}