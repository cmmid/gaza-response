//filter in sidabar
window.onload = function() {
  let widgets = document.querySelectorAll(".lshtm_widget");

  document.querySelectorAll(".lshtm_widget_filter input")
  .forEach(input => {
    input.addEventListener("change", (e) => {
      // Get all checked checkbox values
      let checkedStrata = Array.from(
        document.querySelectorAll(".lshtm_widget_filter input:checked")
      ).map(cb => cb.value);

      // Sort them alphabetically (change this to any other sorting logic if needed)
      checkedStrata.sort();

      // Join with underscore
      let stratum = checkedStrata.length > 0 ? checkedStrata.join("_") : "Overall";
      console.log("Combined stratum string:", stratum);

      widgets.forEach(widget => {
        let widget_id = widget.querySelector(".lshtm_widget_identifier").id;

        widget.querySelector(".visible").classList.remove("visible");
        widget.querySelector("#" + widget_id + "_" + stratum).classList.add("visible");
      });
    });
  });
    document.querySelectorAll(".lshtm_widget_filter input")[0]?.dispatchEvent(new Event('change'));
};
