window.onload = function() {
  let widgets = document.querySelectorAll(".lshtm_widget");
    
  document.querySelectorAll(".lshtm_widget_filter input")
  .forEach(input => {
    input.addEventListener("change", (e) => {
      let stratum = input.value;
      console.log("click "+stratum);
      
      widgets.forEach(widget => {
        let widget_id = widget.querySelector(".lshtm_widget_identifier").id;
        
        widget.querySelector(".visible").classList.remove("visible");
        widget.querySelector("#" + widget_id + "_" + stratum).classList.add("visible");
      });
    });
  });
};