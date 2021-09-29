function display(baseURI) {
  fetch(baseURI + '/Test/helloworld')
  .then(response => {
    return response.text()
  })
  .then(data => {
    alert(data)
  })
  .catch(err => {
    console.log(err)
  })
  
}