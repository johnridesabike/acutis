export async function stringify(data) {
  return JSON.stringify(data, null, 2);
}

export async function another_function() {
  return "success";
}

export async function an_unused_function() {
  return "fail";
}
