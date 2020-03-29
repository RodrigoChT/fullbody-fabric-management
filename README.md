# FullBody Fabric Management System

The **FullBody Fabric Management System** is a Shiny app that was created for the *FullBody Sportswear* textile company to help them manage the production side of their business. The features included have been specifically designed to fit their needs, but many would be useful to similar companies.

## Setup
The setup entails importing separate databases for:

* Supply category structure
* Supplies
* Garments produced with such supplies
* Providers
* Contractors

After the initialization of the system all of these databases can be modified and new items can be created through the app in a secure manner, meaning that the data types and formats are checked and fixed before modifying the databases.

## Features

### Operations
The app is designed around performing various *operations*. These *operations* have a series of checks and are carried out in such a way that many human errors that are common when managing this type of data on spreadsheets are completely impossible to commit. The operations included in the current version of the app are:

* Managing and registering the reception of supplies
* Creating and administering **Product Orders**. These are agreemnets between the company and a contractor for fabricating a set quantity of pieces of clothing. It contains information about the supplies provided to the other party, the labour cost, how much of the order has been received, how much has been rejected on the basis of quality, among other things.
* Supplies inventory management

Each of these *operations* have a set of data fields that the user should fill out by either selecting an option from a dropdown list or by typing the appropiate information. These fields could be the date of the operation, the specific garment design in question, the contractor, the reason for returning a piece of clothing to the contractor, among many others.

### Reporting and visualization
The app also serves as a reporting and visualization tool. All the databases in the **Setup** section as well as a detalided **Supplies Inventory**, **Received garments Inventory**, and a **Movements** table that contains all the performed *operations* are visible inside the app. Furthermore, the per unit cost incurred in each **Product Order** is calculated and shown in a separate **Garment Costs** table. This table allows the user to compare costs across contractors, dates, and types of garments. All tables are searchable and can be downloaded as a csv, excel, or pdf file.

Finally, besides being registered in the **Movements** table, all *operations* generate a pdf document with all their relevant details. See the **Reportes** directory for an example of such a document.

---

### Notes on the repository

* The *Demo* directory contains example images of some of the aspects described in this document.
* The files in the **Data** directory have been removed to safeguard the privacy of *FullBody Sportswear*.
