
function isDescendent(person, ancestor) {

  for (parent in person.parents) {
    if (parent == ancestor) || isDescendent(parent, ancestor) {
      return true
    }

    return false
}
