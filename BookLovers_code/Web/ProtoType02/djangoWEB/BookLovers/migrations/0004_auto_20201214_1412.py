# Generated by Django 3.0.3 on 2020-12-14 05:12

from django.db import migrations, models


class Migration(migrations.Migration):

    dependencies = [
        ('BookLovers', '0003_auto_20201214_1355'),
    ]

    operations = [
        migrations.AddField(
            model_name='healingbook',
            name='author',
            field=models.TextField(default=''),
        ),
        migrations.AddField(
            model_name='healingbook',
            name='image',
            field=models.TextField(default=''),
        ),
        migrations.AddField(
            model_name='healingbook',
            name='keyword',
            field=models.TextField(default=''),
        ),
        migrations.AddField(
            model_name='healingbook',
            name='publisher',
            field=models.TextField(default=''),
        ),
        migrations.AddField(
            model_name='healingbook',
            name='review',
            field=models.TextField(default=''),
        ),
        migrations.AddField(
            model_name='healingbook',
            name='title',
            field=models.TextField(default=''),
        ),
    ]
