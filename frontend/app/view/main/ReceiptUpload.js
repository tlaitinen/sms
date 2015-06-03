Ext.define('Receipts.view.main.ReceiptUpload', {

    alias: 'widget.receiptupload',
    extend: 'Ext.Panel',

     html: '<input id="receiptupload" type="file" name="files" multiple value="' + __('upload.button') + '"/><div id="progress"></div>',

    uploadHandler: function(e) {
        function parseInfo(name) {
            var delims = [' ', '_', '-'];
            var r = {
                name: '',
                amount:0
            };
            delims.forEach(function(delim) {
                var parts = name.split(delim),
                    indexPart = undefined;
                console.log(parts);
                for (var i = 0; i < parts.length; i++) {
                    var part = parts[i],
                        xs = part.split(",");
                    if (xs.length == 2) {
                        try {
                            console.log(part.replace(",","."));
                            var amount = parseFloat(part.replace(",", "."));
                            if (amount) {
                                r.amount = amount;
                                indexPart = i;
                            }
                        } catch (e) {
                            console.log(e);
                        }
                    }
                }
                if (indexPart != undefined) {
                    parts.splice(indexPart, 1);
                    r.name = parts.join(" ").replace(/\.[^/.]+$/, "");
                }
            });
            if (!r.name) {
                r.name = name;
            }
            console.log(r);
            return r;
        }

        function createProgressBar(file) {

            var now = new Date().getTime();
            var ctrl = {
                label: 'uploadlabel-' + now,
                bar: 'upload-' + now,
                update: function(p) {
                    this.progress.updateProgress(p/100.0, __('upload.uploading'));

                },
                done: function(s, msg) {
                    this.progress.updateProgress(1, msg);
                    var ctrl = this;
                    setTimeout(function() { 
                        $('#' + ctrl.label).remove();
                            }, 5000);
                }
            };

            $('<span>')
                .attr("id", ctrl.label)
                .html(
                    $('<div>')
                        .attr('id', ctrl.bar)
                 ).appendTo('#progress');

            ctrl.progress = Ext.create('Ext.ProgressBar', {
                  renderTo: Ext.get(ctrl.bar),
                  width: '100%'
            });
            
            return ctrl;
        }
        function FileUpload(file) {
            this.ctrl = createProgressBar(file);
            var xhr = new XMLHttpRequest();
            var self = this;
            xhr.upload.addEventListener("progress", function(e) {
                    if (e.lengthComputable) {
                        var percentage = Math.round((e.loaded * 100) / e.total);
                        self.ctrl.update(percentage);
                    }        
                }, false);
            xhr.upload.addEventListener("load", function(e) {
                self.ctrl.update(100);        
                    }, false);
            xhr.open("POST", "backend/upload-files", true);
            var fd = new FormData();
            fd.append('file', file);
            xhr.onreadystatechange = function() {
                if (xhr.readyState == 4) {
                    try {
                        var r = JSON.parse(xhr.responseText);
                        self.ctrl.done(xhr.status, file.name + ' ' + r.result);
                        var info = parseInfo(file.name);

                        var receipt = Ext.create('Receipts.model.receipts',
                            {
                                name: info.name,
                                amount: info.amount,
                                fileId: r.fileId,
                                fileName: file.name,
                                insertionTime: (new Date()).toJSON()
                            });
                        receipt.save({
                            success: function(rec, op) {
                                var r = JSON.parse(op.getResponse().responseText);
                                receipt.setId(r.id);
                                Ext.getStore('receipts').add(receipt);
                            }
                        });

                        

                    } catch (e) {
                        console.log(e);
                    }
                }
            }
            xhr.send(fd);
        }
        var files = e.target.files || e.dataTransfer.files;
        for (var i = 0, f; f = files[i]; i++) {
            new FileUpload(f);
        }        

    },

    onRender: function(ct) {
        this.callParent(arguments);
        document.getElementById('receiptupload')
            .addEventListener("change", this.uploadHandler, false);
    }

});
